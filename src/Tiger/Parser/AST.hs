module Tiger.Parser.AST where

import Data.Foldable (foldl)
import Data.Text.Lazy (pack)
import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)
import Tiger.Common hiding (break, many, try, (<|>))
import Tiger.Data.AST
import Tiger.Parser.Internal

integer :: Parser Integer
integer = parens negative <|> natural
  where
    negative = do
      reservedOp "-"
      n <- natural
      return (-n)

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

----------------------------------------
--- if, while, for, break, let
----------------------------------------
ifThen :: Parser Expr
ifThen =
  IfExpr
    <$> (reserved "if" *> expr)
    <* reserved "then"
    <*> expr
    <*> optionMaybe (try (reserved "else" *> expr))

while :: Parser Expr
while =
  WhileExpr
    <$> (reserved "while" *> expr)
    <* reserved "do"
    <*> expr

for :: Parser Expr
for =
  ForExpr
    <$> (reserved "for" *> identifier)
    <* reservedOp ":="
    <*> expr
    <* reserved "to"
    <*> expr
    <* reserved "do"
    <*> expr

break :: Parser Expr
break = reserved "break" *> return BreakExpr

letIn :: Parser Expr
letIn =
  LetExpr
    <$> (reserved "let" *> decls)
    <* reserved "in"
    <*> expr

exprAtom :: Parser Expr
exprAtom =
  try (parens expr)
    <|> try (reserved "nil" *> return NilExpr)
    <|> try ifThen
    <|> try while
    <|> try for
    <|> try break
    <|> try letIn
    <|> (IntExpr <$> integer)
    <|> (StrExpr <$> stringLiteral)
    <|> (LvExpr <$> lvalue)

term :: Parser Expr
term =
  exprAtom >>= \x ->
    (semi >> SeqExpr . (x :) <$> semiSep1 exprAtom) <|> return x

table :: Operators Expr
table =
  [ [ infixOpAL "*" (ArithBinExpr Mul),
      infixOpAL "/" (ArithBinExpr Div)
    ],
    [ infixOpAL "+" (ArithBinExpr Add),
      infixOpAL "-" (ArithBinExpr Sub)
    ],
    [ infixOpAL "=" (CompExpr Eql),
      infixOpAL "<" (CompExpr Lt),
      infixOpAL "<=" (CompExpr Le),
      infixOpAL ">" (CompExpr Gt),
      infixOpAL ">=" (CompExpr Ge),
      infixOpAL "<>" (CompExpr Neq)
    ],
    [ infixOpAL "&" (BoolBinExpr And)
    ],
    [infixOpAL "|" (BoolBinExpr Or)]
  ]

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

decls :: Parser Decls
decls = many1 decl

program :: Parser Program
program =
  try (Defs <$> decls)
    <|> (EntryPoint <$> expr)

parseExpr :: (MonadCatch m) => SourceName -> String -> m Expr
parseExpr src t = case parse expr src (pack t) of
  Right e -> return e
  Left err -> throwString $ show err
