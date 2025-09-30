module Program.Declaration (declaration) where

import Control.Applicative (Alternative ((<|>)))
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
variable = matchTokenType T.VAR *> (withInitializer <|> noInitializer) <* matchTokenType T.SEMICOLON

withInitializer :: TokenParser Variable
withInitializer = Variable <$> variableName <*> (Just <$> (matchTokenType T.EQUAL *> expression))

noInitializer :: TokenParser Variable
noInitializer = Variable <$> variableName <*> pure Nothing

variableName :: TokenParser String
variableName = do
  Token {tokenType = T.IDENTIFIER name} <- satisfy (T.isIdentifier . tokenType) "Expect identifier."
  pure name
