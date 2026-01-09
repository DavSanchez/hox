{-# LANGUAGE UndecidableInstances #-}

module Language.Syntax.Program
  ( Program (..),
    parseProgram,
    Declaration (..),
    Statement (..),
    Variable (..),
    Function (..),
    Class (..),
    lookupMethod,
    FunctionKind (..),
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (when)
import Data.Either (lefts, rights)
import Data.Functor (void, ($>))
import Data.Map qualified as M
import Language.Parser (ParseError, Parser (..), TokenParser, consume, peek, satisfy)
import Language.Syntax.Expression (Expression (..), Literal (Bool), NotResolved (..), Phase (..), expression)
import Language.Syntax.Token (Token (..), TokenType (..), displayTokenType, isIdentifier)

newtype Program (p :: Phase) = Program [Declaration p]

deriving stock instance (Show (Expression p)) => Show (Program p)

deriving stock instance (Eq (Expression p)) => Eq (Program p)

data Declaration (p :: Phase)
  = ClassDecl (Class p)
  | Fun (Function p)
  | VarDecl (Variable p)
  | Statement (Statement p)

deriving stock instance (Eq (Expression p)) => Eq (Declaration p)

deriving stock instance (Show (Expression p)) => Show (Declaration p)

data Class (p :: Phase) = Class
  { className :: String,
    classMethods :: M.Map String (Function p),
    classLine :: Int,
    superClass :: Maybe (Expression p)
  }

deriving stock instance (Eq (Expression p)) => Eq (Class p)

deriving stock instance (Show (Expression p)) => Show (Class p)

lookupMethod :: String -> Class 'Resolved -> Maybe (Function 'Resolved)
lookupMethod methodName cls = M.lookup methodName (classMethods cls)

type Block (p :: Phase) = [Declaration p]

data Function (p :: Phase) = Function
  { funcName :: String,
    funcParams :: [(String, Int)],
    funcBody :: Block p,
    funcLine :: Int
  }

deriving stock instance (Eq (Expression p)) => Eq (Function p)

deriving stock instance (Show (Expression p)) => Show (Function p)

data Variable (p :: Phase) = Variable
  { varName :: String,
    varInitializer :: Maybe (Expression p),
    varLine :: Int
  }

deriving stock instance (Eq (Expression p)) => Eq (Variable p)

deriving stock instance (Show (Expression p)) => Show (Variable p)

data Statement (p :: Phase)
  = ExprStmt (Expression p)
  | IfStmt
      -- | if
      (Expression p)
      -- | then
      (Statement p)
      -- | else
      (Maybe (Statement p))
  | PrintStmt (Expression p)
  | ReturnStmt Int (Maybe (Expression p))
  | WhileStmt (Expression p) (Statement p)
  | BlockStmt (Block p)

deriving stock instance (Eq (Expression p)) => Eq (Statement p)

deriving stock instance (Show (Expression p)) => Show (Statement p)

parseProgram :: [Token] -> Either [ParseError] (Program 'Unresolved)
parseProgram tokens =
  let results = parseProgram' tokens
      errors = lefts results -- Collection of errors
      declarations = rights results -- Parsed program
   in if null errors
        then Right (Program declarations)
        else Left errors

parseProgram' :: [Token] -> [Either ParseError (Declaration 'Unresolved)]
parseProgram' [] = []
parseProgram' [Token {tokenType = EOF}] = []
parseProgram' tokens = case runParser declaration tokens of
  (Left err, rest) -> Left err : parseProgram' (synchronize rest)
  (Right stmt, rest) -> Right stmt : parseProgram' rest

-- | Drop the current token and keep going until we find a statement start
synchronize :: [Token] -> [Token]
synchronize [] = []
synchronize (Token {tokenType = SEMICOLON} : tt) = tt -- Statement boundary after semicolon, stop
synchronize (t : tt) =
  if tokenType t `elem` [CLASS, FUN, VAR, FOR, IF, WHILE, PRINT, RETURN]
    then t : tt -- Statement boundary start, return it
    else synchronize tt

declaration :: TokenParser (Declaration 'Unresolved)
declaration = do
  t <- peek
  case tokenType t of
    CLASS -> ClassDecl <$> classDeclaration
    FUN -> Fun <$> function KFunction
    VAR -> VarDecl <$> variable
    _ -> Statement <$> statement

classDeclaration :: TokenParser (Class 'Unresolved)
classDeclaration = do
  void $ satisfy ((CLASS ==) . tokenType) ("Expect " <> displayTokenType CLASS <> ".")
  Token {tokenType = IDENTIFIER name, line = l} <- satisfy (isIdentifier . tokenType) "Expect class name."
  superClassName <- parseSuperClass
  void $ satisfy ((LEFT_BRACE ==) . tokenType) "Expect '{' before class body."
  methods <- parseClassMethods
  void $ satisfy ((RIGHT_BRACE ==) . tokenType) "Expect '}' after class body."
  pure $ Class name methods l superClassName

parseSuperClass :: TokenParser (Maybe (Expression 'Unresolved))
parseSuperClass = do
  t <- peek
  if tokenType t == LESS
    then do
      void $ satisfy ((LESS ==) . tokenType) ("Expect " <> displayTokenType LESS <> " before superclass name.")
      Token {tokenType = IDENTIFIER superName} <- satisfy (isIdentifier . tokenType) "Expect superclass name."
      pure (Just $ VariableExpr 0 superName NotResolved)
    else pure Nothing

parseClassMethods :: TokenParser (M.Map String (Function 'Unresolved))
parseClassMethods = do
  t <- peek
  if tokenType t == RIGHT_BRACE
    then pure mempty
    else do
      func <- function KMethod
      M.insert (funcName func) func <$> parseClassMethods

data FunctionKind
  = KFunction
  | KMethod
  deriving stock (Eq)

displayFunctionKind :: FunctionKind -> String
displayFunctionKind KFunction = "function"
displayFunctionKind KMethod = "method"

function :: FunctionKind -> TokenParser (Function 'Unresolved)
function kind = do
  when (kind == KFunction) $ void $ satisfy ((FUN ==) . tokenType) ("Expect " <> displayTokenType FUN <> ".")
  Token {tokenType = IDENTIFIER name, line = l} <- satisfy (isIdentifier . tokenType) ("Expect " <> displayFunctionKind kind <> " name.")
  void $ satisfy ((LEFT_PAREN ==) . tokenType) ("Expect '(' after " <> displayFunctionKind kind <> " name.")
  params <- parseFunctionParameters
  when (length params >= 255) $ fail "Can't have more than 255 parameters."
  void $ satisfy ((RIGHT_PAREN ==) . tokenType) "Expect ')' after parameters."
  void $ satisfy ((LEFT_BRACE ==) . tokenType) ("Expect '{' before " <> displayFunctionKind kind <> " body.") -- start function body
  body <- parseScopedProgram
  pure $ Function name params body l

parseFunctionParameters :: TokenParser [(String, Int)]
parseFunctionParameters = do
  t <- peek
  if tokenType t == RIGHT_PAREN
    then pure []
    else go 0 []
  where
    go :: Int -> [(String, Int)] -> TokenParser [(String, Int)]
    go n acc = do
      -- Check limit before consuming the next parameter so the error is emitted
      -- for the offending identifier token.
      if n >= 255
        then fail "Can't have more than 255 parameters."
        else do
          Token {tokenType = IDENTIFIER paramName, line = l} <- satisfy (isIdentifier . tokenType) "Expect parameter name."
          t <- peek
          case tokenType t of
            RIGHT_PAREN -> pure (reverse ((paramName, l) : acc))
            COMMA -> do
              void $ satisfy ((COMMA ==) . tokenType) ("Expect " <> displayTokenType COMMA <> ".")
              go (n + 1) ((paramName, l) : acc)
            _ -> fail "Expect ')' after parameters."

variable :: TokenParser (Variable 'Unresolved)
variable = do
  void $ satisfy ((VAR ==) . tokenType) ("Expect " <> displayTokenType VAR <> ".")
  (name, l) <- variableName
  initExpr <- withInitializer <|> noInitializer
  void varDeclEnd
  pure $ Variable name initExpr l

variable' :: TokenParser (Variable 'Unresolved)
variable' = do
  (name, l) <- variableName
  initExpr <- withInitializer <|> noInitializer
  pure $ Variable name initExpr l

withInitializer :: TokenParser (Maybe (Expression 'Unresolved))
withInitializer = Just <$> (satisfy ((EQUAL ==) . tokenType) ("Expect " <> displayTokenType EQUAL <> ".") *> expression)

noInitializer :: TokenParser (Maybe (Expression 'Unresolved))
noInitializer = pure Nothing

variableName :: TokenParser (String, Int)
variableName = do
  Token {tokenType = IDENTIFIER name, line = l} <- satisfy (isIdentifier . tokenType) "Expect variable name."
  pure (name, l)

varDeclEnd :: TokenParser Token
varDeclEnd = satisfy ((SEMICOLON ==) . tokenType) "Expect ';' after variable declaration"

statement :: TokenParser (Statement 'Unresolved)
statement = do
  t <- peek
  case tokenType t of
    IF -> parseIfStmt
    FOR -> parseForStmt
    PRINT -> parsePrintStmt
    RETURN -> parseReturnStmt
    WHILE -> parseWhileStmt
    LEFT_BRACE -> parseBlockStmt
    _ -> parseExprStmt

parseReturnStmt :: TokenParser (Statement 'Unresolved)
parseReturnStmt = do
  Token {line = l} <- satisfy ((RETURN ==) . tokenType) ("Expect " <> displayTokenType RETURN <> ".")
  t <- peek
  case tokenType t of
    SEMICOLON -> consume $> ReturnStmt l Nothing
    _ -> do
      expr <- expression
      void $ satisfy ((SEMICOLON ==) . tokenType) ("Expect '" <> displayTokenType SEMICOLON <> "' after return value.")
      pure (ReturnStmt l (Just expr))

parseForStmt :: TokenParser (Statement 'Unresolved)
parseForStmt = do
  void $ satisfy ((FOR ==) . tokenType) ("Expect " <> displayTokenType FOR <> ".")
  (initializer, condition, increment) <- parseForComponents
  body <- statement

  let -- body with increment?
      body' = case increment of
        Nothing -> body
        Just cond -> BlockStmt $ Statement <$> [body, ExprStmt cond]
      -- body with condition?
      cond' = case condition of
        Just c -> c
        Nothing -> Literal (Bool True)
      body'' = WhileStmt cond' body'
      -- body with initializer?
      body''' = case initializer of
        Nothing -> body''
        Just initr -> BlockStmt [initr, Statement body'']
  pure body'''

parseForComponents ::
  TokenParser
    ( Maybe (Declaration 'Unresolved),
      Maybe (Expression 'Unresolved),
      Maybe (Expression 'Unresolved)
    )
parseForComponents = do
  void $ satisfy ((LEFT_PAREN ==) . tokenType) "Expect '(' after 'for'."
  initializer <- parseForInitializer
  void $ satisfy ((SEMICOLON ==) . tokenType) "Expect ';' after for initializer."
  condition <- parseForCondition
  void $ satisfy ((SEMICOLON ==) . tokenType) "Expect ';' after for condition."
  increment <- parseForIncrement
  void $ satisfy ((RIGHT_PAREN ==) . tokenType) "Expect ')' after for clauses."
  pure (initializer, condition, increment)

parseForInitializer :: TokenParser (Maybe (Declaration 'Unresolved))
parseForInitializer = do
  peek
    >>= ( \case
            SEMICOLON -> pure Nothing
            VAR -> consume *> (Just . VarDecl <$> variable')
            _ -> Just . Statement . ExprStmt <$> expression
        )
      . tokenType

parseForCondition :: TokenParser (Maybe (Expression 'Unresolved))
parseForCondition = do
  peek
    >>= ( \case
            SEMICOLON -> pure Nothing
            _ -> Just <$> expression
        )
      . tokenType

parseForIncrement :: TokenParser (Maybe (Expression 'Unresolved))
parseForIncrement =
  peek
    >>= ( \case
            RIGHT_PAREN -> pure Nothing
            _ -> Just <$> expression
        )
      . tokenType

parseExprStmt :: TokenParser (Statement 'Unresolved)
parseExprStmt = ExprStmt <$> expression <* satisfy ((SEMICOLON ==) . tokenType) ("Expect '" <> displayTokenType SEMICOLON <> "' after expression.")

parseIfStmt :: TokenParser (Statement 'Unresolved)
parseIfStmt = do
  void $ satisfy ((IF ==) . tokenType) ("Expect " <> displayTokenType IF <> ".")
  void $ satisfy ((LEFT_PAREN ==) . tokenType) "Expect '(' after 'if'."
  expr <- expression
  void $ satisfy ((RIGHT_PAREN ==) . tokenType) "Expect ')' after if condition."
  thenBranch <- statement
  t <- peek
  if tokenType t == ELSE
    then IfStmt expr thenBranch . Just <$> (consume *> statement)
    else pure $ IfStmt expr thenBranch Nothing

parsePrintStmt :: TokenParser (Statement 'Unresolved)
parsePrintStmt = satisfy ((PRINT ==) . tokenType) ("Expect " <> displayTokenType PRINT <> ".") *> (PrintStmt <$> expression) <* satisfy ((SEMICOLON ==) . tokenType) ("Expect " <> displayTokenType SEMICOLON <> ".")

parseWhileStmt :: TokenParser (Statement 'Unresolved)
parseWhileStmt = do
  void $ satisfy ((WHILE ==) . tokenType) ("Expect " <> displayTokenType WHILE <> ".")
  void $ satisfy ((LEFT_PAREN ==) . tokenType) "Expect '(' after 'while'."
  expr <- expression
  void $ satisfy ((RIGHT_PAREN ==) . tokenType) "Expect ')' after condition."
  WhileStmt expr <$> statement

parseBlockStmt :: TokenParser (Statement 'Unresolved)
parseBlockStmt = satisfy ((LEFT_BRACE ==) . tokenType) ("Expect " <> displayTokenType LEFT_BRACE <> ".") *> (BlockStmt <$> parseScopedProgram)

parseScopedProgram :: TokenParser [Declaration 'Unresolved]
parseScopedProgram = Parser $ \tokens -> go tokens []
  where
    go :: [Token] -> [Declaration 'Unresolved] -> (Either ParseError [Declaration 'Unresolved], [Token])
    go [] decls = (Right (reverse decls), [])
    go (Token {tokenType = RIGHT_BRACE} : rest) decls = (Right (reverse decls), rest)
    go ts decls = case runParser declaration ts of
      (Left err, rest) -> (Left err, skipToClose rest)
      (Right decl, rest) -> go rest (decl : decls)

skipToClose :: [Token] -> [Token]
skipToClose [] = []
skipToClose (Token {tokenType = RIGHT_BRACE} : xs) = xs
skipToClose (Token {tokenType = LEFT_BRACE} : xs) = skipToClose xs
skipToClose (_ : xs) = skipToClose xs
