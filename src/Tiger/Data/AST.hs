module Tiger.Data.AST where

import Tiger.Common

-- synonyms
type Id = String

type TypId = String

type TypField = [(Id, TypId)]

-- expr
data Expr
  = LvExpr LValue
  | NilExpr
  | SeqExpr [Expr]
  | IntExpr Integer
  | StrExpr String
  | CallExpr Id [Expr]
  | ArithBinOpExpr ArithBinOp Expr Expr
  | BoolBinOpExpr BoolBinOp Expr Expr
  | IfExpr Expr Expr (Maybe Expr)
  | WhileExpr Expr Expr
  | ForExpr Id Expr Expr Expr
  | BreakExpr
  | LetExpr Decls Expr

data LValue
  = LvVar Id
  | LvField LValue Id
  | LvIndex LValue Expr

data ArithBinOp = Add | Sub | Mul | Div

data BoolBinOp = Eql | GT | GE | LT | LE | Neq | And | Or

-- type
data Typ
  = TypVar TypId
  | TypField TypField
  | TypArray TypId

-- declaration
data Decl
  = VarDecl Id (Maybe TypId) Expr
  | TypDecl TypId Typ
  | FunDecl Id TypField (Maybe TypId) Expr

type Decls = [Decl]
