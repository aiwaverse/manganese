open Types

type expr =
  | Number of int
  | Boolean of bool
  | If of expr * expr * expr
  | Function of string * l1Type * expr
  | App of expr * expr
  | AppOp of operator * expr * expr
  | Let of expr * l1Type * expr
  | LetRec of expr * l1Type * expr
  | Var of string
  | Tuple of expr * expr
  | Fst of expr
  | Snd of expr

and operator = Add | Sub | Mul | Div | LT | LTE | GT | GTE | EQ | And | Or
