module Parser where

import Types

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

integer :: Parser Integer
integer = lexeme L.decimal

intExp = 
   do i <- integer
      return $ IntExp i

rws :: [String] -- list of reserved words
rws = ["if","then","else","fi","let","in","end","False","True"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

varExp :: Parser Exp
varExp = do i <- identifier
            return $ VarExp i

letExp :: Parser Exp
letExp = do rword "let"
            v <- identifier
            symbol "="
            e1 <- anExp
            rword "in"
            e2 <- anExp
            rword "end"
            return $ LetExp v e1 e2

ifExp :: Parser Exp
ifExp = do rword "if"
           c <- anExp
           rword "then"
           e1 <- anExp
           rword "else"
           e2 <- anExp
           return $ IfExp c e1 e2

-- Copied from https://hackage.haskell.org/package/megaparsec-6.4.0/docs/Text-Megaparsec-Expr.html
notFollowedByEq n = (lexeme . try) (string n <* notFollowedBy (symbol "="))

arOperators :: [[Operator Parser Exp]]
arOperators =
  [ [ InfixL (IntOpExp "*" <$ symbol "*")
    , InfixL (IntOpExp "/" <$ notFollowedByEq "/") ]
  , [ InfixL (IntOpExp "+" <$ symbol "+")
    , InfixL (IntOpExp "-" <$ symbol "-") ]
  , [ InfixN (RelOpExp "<=" <$ symbol "<=")
    , InfixN (RelOpExp "<" <$ symbol "<")
    , InfixN (RelOpExp ">=" <$ symbol ">=")
    , InfixN (RelOpExp ">" <$ symbol ">")
    , InfixN (RelOpExp "==" <$ symbol "==")
    , InfixN (RelOpExp "/=" <$ symbol "/=") ]
  , [ InfixL (BoolOpExp "&&" <$ symbol "&&")
    , InfixL (BoolOpExp "||" <$ symbol "||") ]
  ]


arTerm :: Parser Exp
arTerm = (BoolExp True <$ rword "True")
     <|> (BoolExp False <$ rword "False")
     <|> intExp 
     <|> letExp
     <|> ifExp
     <|> varExp

arExp :: Parser Exp
arExp = makeExprParser arTerm arOperators

anExp :: Parser Exp
anExp = arExp

mainParser = anExp
