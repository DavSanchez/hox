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
import Data.Functor (void, ($>))
import Expression (Expression (Literal), Literal (Bool), expression)
import Parser (ParseError, Parser (..), TokenParser, peekToken, satisfy)
import Token (Token (..), displayTokenType)
import Token qualified as T

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
parseProgram' [] = [] -- Should not happen, as we always expect at least EOF
parseProgram' [Token {tokenType = T.EOF}] = []
parseProgram' tokens = case runParser declaration tokens of
  Left err -> Left err : parseProgram' (synchronize tokens)
  Right (stmt, rest) -> Right stmt : parseProgram' rest

-- | Drop the current token and keep going until we find a statement start
synchronize :: [Token] -> [Token]
synchronize s = dropWhile (not . isStmtStart) (drop 1 s)
  where
    isStmtStart :: Token -> Bool
    isStmtStart Token {tokenType = T.CLASS} = True
    isStmtStart Token {tokenType = T.FUN} = True
    isStmtStart Token {tokenType = T.VAR} = True
    isStmtStart Token {tokenType = T.FOR} = True
    isStmtStart Token {tokenType = T.IF} = True
    isStmtStart Token {tokenType = T.WHILE} = True
    isStmtStart Token {tokenType = T.PRINT} = True
    isStmtStart Token {tokenType = T.RETURN} = True
    isStmtStart _ = False

declaration :: TokenParser Declaration
declaration = do
  t <- peekToken
  case tokenType t of
    T.VAR -> VarDecl <$> variable
    _ -> Statement <$> statement

variable :: TokenParser Variable
variable =
  satisfy ((T.VAR ==) . tokenType) ("Expect " <> displayTokenType T.VAR <> ".")
    *> (Variable <$> variableName <*> (withInitializer <|> noInitializer))
    <* varDeclEnd

withInitializer :: TokenParser (Maybe Expression)
withInitializer = Just <$> (satisfy ((T.EQUAL ==) . tokenType) ("Expect " <> displayTokenType T.EQUAL <> ".") *> expression)

noInitializer :: TokenParser (Maybe Expression)
noInitializer = pure Nothing

variableName :: TokenParser String
variableName = do
  Token {tokenType = T.IDENTIFIER name} <- satisfy (T.isIdentifier . tokenType) "Expect variable name."
  pure name

varDeclEnd :: TokenParser Token
varDeclEnd = satisfy ((T.SEMICOLON ==) . tokenType) "Expect ';' after variable declaration"

statement :: TokenParser Statement
statement = do
  t <- peekToken
  case tokenType t of
    T.IF -> parseIfStmt
    T.FOR -> parseForStmt
    T.PRINT -> parsePrintStmt
    T.WHILE -> parseWhileStmt
    T.LEFT_BRACE -> parseBlockStmt
    _ -> parseExprStmt

parseForStmt :: TokenParser Statement
parseForStmt = do
  void $ satisfy ((T.FOR ==) . tokenType) ("Expect " <> displayTokenType T.FOR <> ".")
  void $ satisfy ((T.LEFT_PAREN ==) . tokenType) "Expect '(' after 'for'."
  initializer <- parseForInitializer
  condition <- parseForCondition
  void $ satisfy ((T.SEMICOLON ==) . tokenType) "Expect ';' after loop condition."
  increment <- parseForIncrement
  void $ satisfy ((T.RIGHT_PAREN ==) . tokenType) "Expect ')' after for clauses."
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

parseForInitializer :: TokenParser (Maybe Declaration)
parseForInitializer =
  (satisfy ((T.SEMICOLON ==) . tokenType) "Expect ';' after for initializer." $> Nothing)
    <|> satisfy ((T.VAR ==) . tokenType) ("Expect " <> displayTokenType T.VAR <> " or ';' after for initializer.") *> (Just . VarDecl <$> variable)
    <|> Just . Statement <$> parseExprStmt

parseForCondition :: TokenParser (Maybe Expression)
parseForCondition =
  satisfy ((T.SEMICOLON ==) . tokenType) "Expect ';' after loop condition." $> Nothing
    <|> Just <$> expression

parseForIncrement :: TokenParser (Maybe Expression)
parseForIncrement =
  satisfy ((T.RIGHT_PAREN ==) . tokenType) "Expect ')' after for clauses." $> Nothing
    <|> Just <$> expression

parseExprStmt :: TokenParser Statement
parseExprStmt = ExprStmt <$> expression <* satisfy ((T.SEMICOLON ==) . tokenType) ("Expect " <> displayTokenType T.SEMICOLON <> ".")

parseIfStmt :: TokenParser Statement
parseIfStmt = do
  void $ satisfy ((T.IF ==) . tokenType) ("Expect " <> displayTokenType T.IF <> ".")
  void $ satisfy ((T.LEFT_PAREN ==) . tokenType) "Expect '(' after 'if'."
  expr <- expression
  void $ satisfy ((T.RIGHT_PAREN ==) . tokenType) "Expect ')' after if condition."
  thenBranch <- statement
  t <- peekToken
  if tokenType t == T.ELSE
    then IfStmt expr thenBranch . Just <$> statement
    else pure $ IfStmt expr thenBranch Nothing

parsePrintStmt :: TokenParser Statement
parsePrintStmt = satisfy ((T.PRINT ==) . tokenType) ("Expect " <> displayTokenType T.PRINT <> ".") *> (PrintStmt <$> expression) <* satisfy ((T.SEMICOLON ==) . tokenType) ("Expect " <> displayTokenType T.SEMICOLON <> ".")

parseWhileStmt :: TokenParser Statement
parseWhileStmt = do
  void $ satisfy ((T.WHILE ==) . tokenType) ("Expect " <> displayTokenType T.WHILE <> ".")
  void $ satisfy ((T.LEFT_PAREN ==) . tokenType) "Expect '(' after 'while'."
  expr <- expression
  void $ satisfy ((T.RIGHT_PAREN ==) . tokenType) "Expect ')' after condition."
  WhileStmt expr <$> statement

parseBlockStmt :: TokenParser Statement
parseBlockStmt = satisfy ((T.LEFT_BRACE ==) . tokenType) ("Expect " <> displayTokenType T.LEFT_BRACE <> ".") *> (BlockStmt <$> parseScopedProgram)

-- TODO review
parseScopedProgram :: TokenParser [Declaration]
parseScopedProgram = Parser $ \tokens -> go tokens []
  where
    go :: [Token] -> [Declaration] -> Either ParseError ([Declaration], [Token])
    go [] decls = Right (reverse decls, [])
    go (Token {tokenType = T.RIGHT_BRACE} : rest) decls = Right (reverse decls, rest)
    go ts decls = case runParser declaration ts of
      Left err -> Left err
      Right (decl, rest) -> go rest (decl : decls)
