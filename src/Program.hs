module Program (Program (..), parseProgram, Declaration (..), Statement (..), Variable (..)) where

import Control.Applicative (Alternative ((<|>)))
import Data.Either (lefts, rights)
import Expression (Expression, expression)
import Parser (ParseError, Parser (..), TokenParser, matchTokenType, peekToken, satisfy)
import Token (Token (..))
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
  | PrintStmt Expression
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
variable = matchTokenType T.VAR *> (Variable <$> variableName <*> (withInitializer <|> noInitializer)) <* varDeclEnd

withInitializer :: TokenParser (Maybe Expression)
withInitializer = Just <$> (matchTokenType T.EQUAL *> expression)

noInitializer :: TokenParser (Maybe Expression)
noInitializer = pure Nothing

variableName :: TokenParser String
variableName = do
  Token {tokenType = T.IDENTIFIER name} <- satisfy (T.isIdentifier . tokenType) "Expect variable name."
  pure name

varDeclEnd :: TokenParser Token
varDeclEnd = satisfy (\t -> tokenType t == T.SEMICOLON) "Expect ';' after variable declaration"

statement :: TokenParser Statement
statement = do
  t <- peekToken
  case tokenType t of
    T.PRINT -> parsePrintStmt
    T.LEFT_BRACE -> parseBlockStmt
    _ -> parseExprStmt

parseExprStmt :: TokenParser Statement
parseExprStmt = ExprStmt <$> expression <* matchTokenType T.SEMICOLON

parsePrintStmt :: TokenParser Statement
parsePrintStmt = matchTokenType T.PRINT *> (PrintStmt <$> expression) <* matchTokenType T.SEMICOLON

parseBlockStmt :: TokenParser Statement
parseBlockStmt = matchTokenType T.LEFT_BRACE *> (BlockStmt <$> parseScopedProgram)

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
