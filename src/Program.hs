module Program
  ( Program (..),
    parseProgram,
    Declaration (..),
    Statement (..),
    Variable (..),
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.Either (lefts, rights)
import Data.Functor (void)
import Expression (Expression (Literal), Literal (Bool), expression)
import Parser (ParseError, Parser (..), TokenParser, consume, peek, satisfy)
import Token (Token (..), TokenType (..), displayTokenType, isIdentifier)

newtype Program = Program [Declaration] deriving stock (Show)

data Declaration = VarDecl Variable | Statement Statement deriving stock (Show, Eq)

data Variable = Variable
  { varName :: String,
    varInitializer :: Maybe Expression
  }
  deriving stock (Show, Eq)

data Statement
  = ExprStmt Expression
  | IfStmt
      -- | if
      Expression
      -- | then
      Statement
      -- | else
      (Maybe Statement)
  | PrintStmt Expression
  | WhileStmt Expression Statement
  | BlockStmt [Declaration]
  deriving stock (Show, Eq)

parseProgram :: [Token] -> Either [ParseError] Program
parseProgram tokens =
  let results = parseProgram' tokens
      errors = lefts results -- Collection of errors
      declarations = rights results -- Parsed program
   in if null errors
        then Right (Program declarations)
        else Left errors

parseProgram' :: [Token] -> [Either ParseError Declaration]
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

declaration :: TokenParser Declaration
declaration = do
  t <- peek
  case tokenType t of
    VAR -> VarDecl <$> variable
    _ -> Statement <$> statement

variable :: TokenParser Variable
variable =
  satisfy ((VAR ==) . tokenType) ("Expect " <> displayTokenType VAR <> ".")
    *> (Variable <$> variableName <*> (withInitializer <|> noInitializer))
    <* varDeclEnd

variable' :: TokenParser Variable
variable' = Variable <$> variableName <*> (withInitializer <|> noInitializer)

withInitializer :: TokenParser (Maybe Expression)
withInitializer = Just <$> (satisfy ((EQUAL ==) . tokenType) ("Expect " <> displayTokenType EQUAL <> ".") *> expression)

noInitializer :: TokenParser (Maybe Expression)
noInitializer = pure Nothing

variableName :: TokenParser String
variableName = do
  Token {tokenType = IDENTIFIER name} <- satisfy (isIdentifier . tokenType) "Expect variable name."
  pure name

varDeclEnd :: TokenParser Token
varDeclEnd = satisfy ((SEMICOLON ==) . tokenType) "Expect ';' after variable declaration"

statement :: TokenParser Statement
statement = do
  t <- peek
  case tokenType t of
    IF -> parseIfStmt
    FOR -> parseForStmt
    PRINT -> parsePrintStmt
    WHILE -> parseWhileStmt
    LEFT_BRACE -> parseBlockStmt
    _ -> parseExprStmt

parseForStmt :: TokenParser Statement
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

parseForComponents :: TokenParser (Maybe Declaration, Maybe Expression, Maybe Expression)
parseForComponents = do
  void $ satisfy ((LEFT_PAREN ==) . tokenType) "Expect '(' after 'for'."
  initializer <- parseForInitializer
  void $ satisfy ((SEMICOLON ==) . tokenType) "Expect ';' after for initializer."
  condition <- parseForCondition
  void $ satisfy ((SEMICOLON ==) . tokenType) "Expect ';' after for condition."
  increment <- parseForIncrement
  void $ satisfy ((RIGHT_PAREN ==) . tokenType) "Expect ')' after for clauses."
  pure (initializer, condition, increment)

parseForInitializer :: TokenParser (Maybe Declaration)
parseForInitializer = do
  peek
    >>= ( \case
            SEMICOLON -> pure Nothing
            VAR -> consume *> (Just . VarDecl <$> variable')
            _ -> Just . Statement . ExprStmt <$> expression
        )
      . tokenType

parseForCondition :: TokenParser (Maybe Expression)
parseForCondition = do
  peek
    >>= ( \case
            SEMICOLON -> pure Nothing
            _ -> Just <$> expression
        )
      . tokenType

parseForIncrement :: TokenParser (Maybe Expression)
parseForIncrement =
  peek
    >>= ( \case
            RIGHT_PAREN -> pure Nothing
            _ -> Just <$> expression
        )
      . tokenType

parseExprStmt :: TokenParser Statement
parseExprStmt = ExprStmt <$> expression <* satisfy ((SEMICOLON ==) . tokenType) ("Expect '" <> displayTokenType SEMICOLON <> "' after expression.")

parseIfStmt :: TokenParser Statement
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

parsePrintStmt :: TokenParser Statement
parsePrintStmt = satisfy ((PRINT ==) . tokenType) ("Expect " <> displayTokenType PRINT <> ".") *> (PrintStmt <$> expression) <* satisfy ((SEMICOLON ==) . tokenType) ("Expect " <> displayTokenType SEMICOLON <> ".")

parseWhileStmt :: TokenParser Statement
parseWhileStmt = do
  void $ satisfy ((WHILE ==) . tokenType) ("Expect " <> displayTokenType WHILE <> ".")
  void $ satisfy ((LEFT_PAREN ==) . tokenType) "Expect '(' after 'while'."
  expr <- expression
  void $ satisfy ((RIGHT_PAREN ==) . tokenType) "Expect ')' after condition."
  WhileStmt expr <$> statement

parseBlockStmt :: TokenParser Statement
parseBlockStmt = satisfy ((LEFT_BRACE ==) . tokenType) ("Expect " <> displayTokenType LEFT_BRACE <> ".") *> (BlockStmt <$> parseScopedProgram)

-- TODO review
parseScopedProgram :: TokenParser [Declaration]
parseScopedProgram = Parser $ \tokens -> go tokens []
  where
    go :: [Token] -> [Declaration] -> (Either ParseError [Declaration], [Token])
    go [] decls = (Right (reverse decls), [])
    go (Token {tokenType = RIGHT_BRACE} : rest) decls = (Right (reverse decls), rest)
    go ts decls = case runParser declaration ts of
      (Left err, rest) -> (Left err, rest)
      (Right decl, rest) -> go rest (decl : decls)
