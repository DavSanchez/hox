module Program.Declaration (Declaration (..), Variable (..), declaration) where

import Control.Applicative (Alternative ((<|>)))
import Environment (Environment)
import Expression (Expression, expression)
import Parser (TokenParser, matchTokenType, satisfy)
import Program.Statement (Statement, statement)
import Token (Token (..))
import Token qualified as T

data Declaration = VarDecl Variable | Statement Statement deriving stock (Show, Eq)

data Variable = Variable
  { varName :: String,
    varInitializer :: Maybe Expression
  }
  deriving stock (Show, Eq)

declaration :: TokenParser Declaration
declaration = VarDecl <$> variable <|> Statement <$> statement

variable :: TokenParser Variable
variable = matchTokenType T.VAR *> (withInitializer <|> noInitializer) <* varDeclEnd

withInitializer :: TokenParser Variable
withInitializer = Variable <$> variableName <*> (Just <$> (matchTokenType T.EQUAL *> expression))

noInitializer :: TokenParser Variable
noInitializer = Variable <$> variableName <*> pure Nothing

variableName :: TokenParser String
variableName = do
  Token {tokenType = T.IDENTIFIER name} <- satisfy (T.isIdentifier . tokenType) "variable name"
  pure name

varDeclEnd :: TokenParser Token
varDeclEnd = satisfy (\t -> tokenType t == T.SEMICOLON) "';' after variable declaration"
