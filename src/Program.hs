module Program
  ( Program (..),
    parseProgram,
    Declaration (..),
    Statement (..),
    Variable (..),
    Function (..),
    Class (..),
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (when)
import Data.Either (lefts, rights)
import Data.Functor (void, ($>))
import Expression (Expression (Literal), Literal (Bool), Phase, Unresolved (..), expression)
import Parser (ParseError, Parser (..), TokenParser, consume, peek, satisfy)
import Token (Token (..), TokenType (..), displayTokenType, isIdentifier)

-- GADTs for AST with phase parameter
data Program a where
  Program :: (Phase a) => [Declaration a] -> Program a

-- Stand-alone deriving instances for the `Program a` GADT
deriving stock instance (Show a) => Show (Program a)

deriving stock instance (Eq a) => Eq (Program a)

data Declaration a
  = ClassDecl (Class a)
  | Fun (Function a)
  | VarDecl (Variable a)
  | Statement (Statement a)
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

data Class a = Class
  { className :: String,
    classMethods :: [Function a],
    classLine :: Int
  }
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

type Block a = [Declaration a]

data Function a = Function
  { funcName :: String,
    funcParams :: [(String, Int)],
    funcBody :: Block a,
    funcLine :: Int
  }
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

data Variable a = Variable
  { varName :: String,
    varInitializer :: Maybe (Expression a),
    varLine :: Int
  }
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

data Statement a
  = ExprStmt (Expression a)
  | IfStmt
      -- | if
      (Expression a)
      -- | then
      (Statement a)
      -- | else
      (Maybe (Statement a))
  | PrintStmt (Expression a)
  | ReturnStmt Int (Maybe (Expression a))
  | WhileStmt (Expression a) (Statement a)
  | BlockStmt (Block a)
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

parseProgram :: [Token] -> Either [ParseError] (Program Unresolved)
parseProgram tokens =
  let results = parseProgram' tokens
      errors = lefts results -- Collection of errors
      declarations = rights results -- Parsed program
   in if null errors
        then Right (Program declarations)
        else Left errors

parseProgram' :: [Token] -> [Either ParseError (Declaration Unresolved)]
parseProgram' [] = []
parseProgram' [Token {tokenType = EOF}] = []
parseProgram' tokens = case runParser declaration tokens of
  (Left err, rest) -> Left err : parseProgram' (synchronize rest)
  (Right stmt, rest) -> Right stmt : parseProgram' rest

-- | Drop the current token and keep going until we find a statement start
synchronize :: [Token] -> [Token]
synchronize [] = []
synchronize [_] = []
synchronize (_ : Token {tokenType = EOF} : _) = [] -- EOF found, stop
synchronize (Token {tokenType = SEMICOLON} : tt) = tt -- Statement boundary after semicolon, stop
synchronize (_ : t : tt) =
  if tokenType t `elem` [CLASS, FUN, VAR, FOR, IF, WHILE, PRINT, RETURN]
    then t : tt -- Statement boundary start, return it
    else synchronize (t : tt)

declaration :: TokenParser (Declaration Unresolved)
declaration = do
  t <- peek
  case tokenType t of
    CLASS -> ClassDecl <$> classDeclaration
    FUN -> Fun <$> function "function"
    VAR -> VarDecl <$> variable
    _ -> Statement <$> statement

classDeclaration :: TokenParser (Class Unresolved)
classDeclaration = do
  void $ satisfy ((CLASS ==) . tokenType) ("Expect " <> displayTokenType CLASS <> ".")
  Token {tokenType = IDENTIFIER name, line = l} <- satisfy (isIdentifier . tokenType) "Expect class name."
  void $ satisfy ((LEFT_BRACE ==) . tokenType) "Expect '{' before class body."
  methods <- parseClassMethods
  void $ satisfy ((RIGHT_BRACE ==) . tokenType) "Expect '}' after class body."
  pure $ Class name methods l

parseClassMethods :: TokenParser [Function Unresolved]
parseClassMethods = do
  t <- peek
  if tokenType t == RIGHT_BRACE
    then pure []
    else do
      func <- function "method"
      rest <- parseClassMethods
      pure (func : rest)

function :: String -> TokenParser (Function Unresolved)
function kind = do
  void $ satisfy ((FUN ==) . tokenType) ("Expect " <> displayTokenType FUN <> ".")
  Token {tokenType = IDENTIFIER name, line = l} <- satisfy (isIdentifier . tokenType) ("Expect " <> kind <> " name.")
  void $ satisfy ((LEFT_PAREN ==) . tokenType) ("Expect '(' after " <> kind <> " name.")
  params <- parseFunctionParameters
  when (length params >= 255) $ fail "Can't have more than 255 parameters."
  void $ satisfy ((RIGHT_PAREN ==) . tokenType) "Expect ')' after parameters."
  void $ satisfy ((LEFT_BRACE ==) . tokenType) ("Expect '{' before " <> kind <> " body.") -- start function body
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

variable :: TokenParser (Variable Unresolved)
variable = do
  void $ satisfy ((VAR ==) . tokenType) ("Expect " <> displayTokenType VAR <> ".")
  (name, l) <- variableName
  initExpr <- withInitializer <|> noInitializer
  void varDeclEnd
  pure $ Variable name initExpr l

variable' :: TokenParser (Variable Unresolved)
variable' = do
  (name, l) <- variableName
  initExpr <- withInitializer <|> noInitializer
  pure $ Variable name initExpr l

withInitializer :: TokenParser (Maybe (Expression Unresolved))
withInitializer = Just <$> (satisfy ((EQUAL ==) . tokenType) ("Expect " <> displayTokenType EQUAL <> ".") *> expression)

noInitializer :: TokenParser (Maybe (Expression Unresolved))
noInitializer = pure Nothing

variableName :: TokenParser (String, Int)
variableName = do
  Token {tokenType = IDENTIFIER name, line = l} <- satisfy (isIdentifier . tokenType) "Expect variable name."
  pure (name, l)

varDeclEnd :: TokenParser Token
varDeclEnd = satisfy ((SEMICOLON ==) . tokenType) "Expect ';' after variable declaration"

statement :: TokenParser (Statement Unresolved)
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

parseReturnStmt :: TokenParser (Statement Unresolved)
parseReturnStmt = do
  Token {line = l} <- satisfy ((RETURN ==) . tokenType) ("Expect " <> displayTokenType RETURN <> ".")
  t <- peek
  case tokenType t of
    SEMICOLON -> consume $> ReturnStmt l Nothing
    _ -> do
      expr <- expression
      void $ satisfy ((SEMICOLON ==) . tokenType) ("Expect '" <> displayTokenType SEMICOLON <> "' after return value.")
      pure (ReturnStmt l (Just expr))

parseForStmt :: TokenParser (Statement Unresolved)
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

parseForComponents :: TokenParser (Maybe (Declaration Unresolved), Maybe (Expression Unresolved), Maybe (Expression Unresolved))
parseForComponents = do
  void $ satisfy ((LEFT_PAREN ==) . tokenType) "Expect '(' after 'for'."
  initializer <- parseForInitializer
  void $ satisfy ((SEMICOLON ==) . tokenType) "Expect ';' after for initializer."
  condition <- parseForCondition
  void $ satisfy ((SEMICOLON ==) . tokenType) "Expect ';' after for condition."
  increment <- parseForIncrement
  void $ satisfy ((RIGHT_PAREN ==) . tokenType) "Expect ')' after for clauses."
  pure (initializer, condition, increment)

parseForInitializer :: TokenParser (Maybe (Declaration Unresolved))
parseForInitializer = do
  peek
    >>= ( \case
            SEMICOLON -> pure Nothing
            VAR -> consume *> (Just . VarDecl <$> variable')
            _ -> Just . Statement . ExprStmt <$> expression
        )
      . tokenType

parseForCondition :: TokenParser (Maybe (Expression Unresolved))
parseForCondition = do
  peek
    >>= ( \case
            SEMICOLON -> pure Nothing
            _ -> Just <$> expression
        )
      . tokenType

parseForIncrement :: TokenParser (Maybe (Expression Unresolved))
parseForIncrement =
  peek
    >>= ( \case
            RIGHT_PAREN -> pure Nothing
            _ -> Just <$> expression
        )
      . tokenType

parseExprStmt :: TokenParser (Statement Unresolved)
parseExprStmt = ExprStmt <$> expression <* satisfy ((SEMICOLON ==) . tokenType) ("Expect '" <> displayTokenType SEMICOLON <> "' after expression.")

parseIfStmt :: TokenParser (Statement Unresolved)
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

parsePrintStmt :: TokenParser (Statement Unresolved)
parsePrintStmt = satisfy ((PRINT ==) . tokenType) ("Expect " <> displayTokenType PRINT <> ".") *> (PrintStmt <$> expression) <* satisfy ((SEMICOLON ==) . tokenType) ("Expect " <> displayTokenType SEMICOLON <> ".")

parseWhileStmt :: TokenParser (Statement Unresolved)
parseWhileStmt = do
  void $ satisfy ((WHILE ==) . tokenType) ("Expect " <> displayTokenType WHILE <> ".")
  void $ satisfy ((LEFT_PAREN ==) . tokenType) "Expect '(' after 'while'."
  expr <- expression
  void $ satisfy ((RIGHT_PAREN ==) . tokenType) "Expect ')' after condition."
  WhileStmt expr <$> statement

parseBlockStmt :: TokenParser (Statement Unresolved)
parseBlockStmt = satisfy ((LEFT_BRACE ==) . tokenType) ("Expect " <> displayTokenType LEFT_BRACE <> ".") *> (BlockStmt <$> parseScopedProgram)

parseScopedProgram :: TokenParser [Declaration Unresolved]
parseScopedProgram = Parser $ \tokens -> go tokens []
  where
    go :: [Token] -> [Declaration Unresolved] -> (Either ParseError [Declaration Unresolved], [Token])
    go [] decls = (Right (reverse decls), [])
    go (Token {tokenType = RIGHT_BRACE} : rest) decls = (Right (reverse decls), rest)
    go ts decls = case runParser declaration ts of
      (Left err, rest) -> (Left err, rest)
      (Right decl, rest) -> go rest (decl : decls)
