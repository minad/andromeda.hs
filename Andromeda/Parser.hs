{-# LANGUAGE TupleSections #-}
module Andromeda.Parser (
  parse
) where

import Text.Parsec.Error
import Andromeda.Common
import Andromeda.Input
import Text.Parsec hiding (parse)
import qualified Text.Parsec.Token as T
import Control.Applicative ((<*>), (<*), (*>), (<$>), (<$))
import Data.Text.Lazy (Text)
import Data.Char (isPunctuation, isSymbol)
import Control.Monad.Identity

type Parser a = Parsec Text () a

isOperator :: Char -> Bool
isOperator c = isPunctuation c || isSymbol c

lexer :: T.GenTokenParser Text u Identity
lexer = T.makeTokenParser T.LanguageDef
        { T.commentStart   = "(*"
        , T.commentEnd     = "*)"
        , T.commentLine    = "(*)"
        , T.nestedComments = True
        , T.identStart     = letter <|> char '_'
        , T.identLetter    = alphaNum <|> oneOf "'_?"
        , T.opStart        = satisfy isOperator
        , T.opLetter       = satisfy isOperator
        , T.reservedOpNames = [".", ":", ",", "->", "=>", ":="]
        , T.reservedNames = ["forall", "fun", "Type", "Check", "Definition", "Eval", "Help", "Parameter", "Context"]
        , T.caseSensitive  = True
        }

reserved, reservedOp :: String -> Parser ()
reserved = T.reserved lexer
reservedOp = T.reservedOp lexer

name :: Parser String
name = T.identifier lexer

dot, comma, colon, whiteSpace :: Parser ()
dot = () <$ T.dot lexer
comma = () <$ T.comma lexer
colon = () <$ T.colon lexer
whiteSpace = T.whiteSpace lexer

natural :: Parser Integer
natural = T.natural lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

directives :: Parser [Directive]
directives = whiteSpace *> many (directive <* dot) <* eof

withLoc :: Parser a -> Parser (a, Location)
withLoc p = do
  start <- getPosition
  x <- p
  end <- getPosition
  return (x, Location (sourceName start) (sourceLine start) (sourceColumn start) (sourceLine end) (sourceColumn end))

directive :: Parser Directive
directive = withLoc (Help <$ reserved "Help" <|>
                     Parameter <$> (reserved "Parameter" *> name) <* colon <*> expr <|>
                     Check <$> (reserved "Check" *> expr) <|>
                     Eval <$> (reserved "Eval" *> expr) <|>
                     Definition <$> (reserved "Definition" *> name) <* reservedOp ":=" <*> expr <|>
                     Context <$ reserved "Context")

expr :: Parser Expr
expr = withLoc expr'

expr' :: Parser Expr'
expr' = fst <$> ((fold Pi <$> (reserved "forall" *> abstraction) <* comma <*> expr) <|>
                 (fold Lambda <$> (reserved "fun" *> abstraction) <* reservedOp "=>" <*> expr)) <|>
           do e <- appExpr
              Pi <$> (("_",e,) <$> (reservedOp "->" *> expr)) <|> return (fst e)

abstraction :: Parser [(([Variable], Expr), Location)]
abstraction = (:[]) <$> bind <|> many1 (parens bind)

bind :: Parser (([Variable], Expr), Location)
bind = withLoc ((,) <$> many1 name <* colon <*> expr)

appExpr :: Parser Expr
appExpr = chainl1 simpleExpr $ return app
    where app x y = (App x y, loc x y)
          loc (_, Location n sl sc _ _) (_, Location _ _ _ el ec) = Location n sl sc el ec
          loc _ _ = Nowhere

simpleExpr :: Parser Expr
simpleExpr = withLoc simpleExpr'

simpleExpr' :: Parser Expr'
simpleExpr' = Universe <$> fromInteger <$> (reserved "Type" *> natural) <|>
              Var <$> name <|>
              parens expr'

fold :: (Abstraction -> Expr') -> [(([Variable], Expr), Location)] -> Expr -> Expr
fold _ [] e = e
fold f (((xs, t), loc):lst) e = foldr (\x g -> (f (x, t, g), loc)) (fold f lst e) xs

parse :: (Functor m, Monad m) => FilePath -> Text -> ErrorT m [Directive]
parse path code = case runParser directives () path code of
  Left e -> let p = errorPos e
                l = sourceLine p
                c = sourceColumn p
                m = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages e)
            in syntaxError (Location (sourceName p) l c l c) (drop 1 m)
  Right e -> return e
