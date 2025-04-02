module Tiger.Parser.Internal
  ( reserved,
    reservedOp,
    identifier,
    identifier',
    parens,
    brackets,
    braces,
    commaSep,
    semi,
    semiSep1,
    natural,
    stringLiteral,
    Operators,
    infixOp,
    infixOpAL,
    contents,
  )
where

import Data.Text.Internal.Lazy (Text)
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Text.Lazy (Parser)
import qualified Text.Parsec.Token as Tok
import Tiger.Common hiding (Op, Text, many, (<|>))

reservedNames :: [String]
reservedNames =
  [ "var",
    "function",
    "type",
    "nil",
    "if",
    "then",
    "else",
    "array",
    "of",
    "while",
    "do",
    "for",
    "to",
    "break",
    "let",
    "in"
  ]

reservedOps :: [String]
reservedOps =
  [ "=>",
    "->",
    "-",
    "*",
    "/",
    "+",
    "<",
    ">",
    ":=",
    ":",
    "=",
    "<>",
    "<=",
    ">=",
    "&",
    "|"
  ]

identLetter :: Parser Char
identLetter = alphaNum <|> oneOf "_'"

lexer :: Tok.GenTokenParser Text () Identity
lexer =
  Tok.makeTokenParser $
    Tok.LanguageDef
      { Tok.commentStart = "{-",
        Tok.commentEnd = "-}",
        Tok.commentLine = "--",
        Tok.nestedComments = True,
        Tok.identStart = lower,
        Tok.identLetter = identLetter,
        Tok.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
        Tok.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
        Tok.reservedNames = reservedNames,
        Tok.reservedOpNames = reservedOps,
        Tok.caseSensitive = True
      }

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

-- not have whitespace just after
identifier' :: Parser String
identifier' = Tok.identifier lexer <* notFollowedBy (oneOf " \t\n")

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semi :: Parser String
semi = Tok.semi lexer

semiSep1 :: Parser a -> Parser [a]
semiSep1 = Tok.semiSep1 lexer

natural :: Parser Integer
natural = Tok.natural lexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer

type Op a = Ex.Operator Text () Identity a

type Operators a = Ex.OperatorTable Text () Identity a

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x op = Ex.Infix (reservedOp x *> return op)

-- infix, left association
infixOpAL :: String -> (a -> a -> a) -> Op a
infixOpAL x op = infixOp x op Ex.AssocLeft

contents :: Operators a -> Parser a -> Parser a
contents = Ex.buildExpressionParser
