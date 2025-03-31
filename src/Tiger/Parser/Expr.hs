module Tiger.Parser.Expr where

import Data.Foldable (foldl)
import Data.Text.Lazy (pack)
import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)
import Tiger.Common hiding (many, try, (<|>))
import Tiger.Data.AST
import Tiger.Parser.Internal

----------------------------------------
--- type
----------------------------------------
typSynonym :: Parser TypId
typSynonym = reservedOp ":" >> identifier

typId :: Parser String
typId = identifier

typField :: Parser TypField
typField = commaSep ((,) <$> identifier <* reservedOp ":" <*> typId)

typ :: Parser Typ
typ =
  try (reserved "array" *> reserved "of" *> (TypArray <$> typId))
    <|> (TypField <$> braces typField)
    <|> (TypVar <$> typId)

----------------------------------------
--- expression
----------------------------------------

-- l-value
lvalue :: Parser LValue
lvalue = do
  base <- baseLValue
  rest <- many extendedLValue
  return (foldl (\lv f -> f lv) base rest)

baseLValue :: Parser LValue
baseLValue = LvVar <$> identifier'

extendedLValue :: Parser (LValue -> LValue)
extendedLValue =
  choice
    [ do
        void $ char '.'
        field <- identifier'
        return (`LvField` field),
      do
        idx <- brackets expr
        return (`LvIndex` idx)
    ]

exprAtom :: Parser Expr
exprAtom =
  parens expr
    <|> (reserved "nil" *> return NilExpr)
    <|> (IntExpr <$> natural)
    <|> (StrExpr <$> stringLiteral)
    <|> (LvExpr <$> lvalue)

term :: Parser Expr
term =
  exprAtom >>= \x ->
    (SeqExpr . (x :) <$> semiSep1 exprAtom) <|> return x

table :: Operators Expr
table = []

-- [ [ infixOpAL "*" (\e1 e2 -> Prim (Arith (*) e1 e2))
--   ],
--   [ infixOpAL "+" (\e1 e2 -> Prim (Arith (+) e1 e2)),
--     infixOpAL "-" (\e1 e2 -> Prim (Arith (-) e1 e2))
--   ],
--   [ infixOpAL "==" (\e1 e2 -> Prim (Comp (==) e1 e2)),
--     infixOpAL "<" (\e1 e2 -> Prim (Comp (<) e1 e2))
--   ]
-- ]

expr :: Parser Expr
expr = contents table term

----------------------------------------
--- declarations
----------------------------------------
decl :: Parser Decl
decl = varDecl <|> funDecl <|> typDecl

varDecl :: Parser Decl
varDecl =
  VarDecl
    <$> (reserved "var" *> identifier)
    <*> optionMaybe (try typSynonym)
    <* reservedOp ":="
    <*> expr

funDecl :: Parser Decl
funDecl =
  FunDecl
    <$> (reserved "function" *> identifier')
    <*> parens typField
    <*> optionMaybe (try typSynonym)
    <* reservedOp "="
    <*> expr

typDecl :: Parser Decl
typDecl =
  TypDecl
    <$> (reserved "type" *> identifier)
    <* reservedOp "="
    <*> typ

parseExpr :: (MonadCatch m) => SourceName -> String -> m Expr
parseExpr src t = case parse expr src (pack t) of
  Right e -> return e
  Left err -> throwString $ show err
