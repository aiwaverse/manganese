open Types

type expr =
  | Number of int
  | Boolean of bool
  | Function of expr * l1Type * expr
  | App of expr * expr
  | AppOp of operator * expr * expr
  | If of expr * expr * expr
  | Let of expr * l1Type * expr
  | Tuple of expr * expr
  | Fst of expr
  | Snd of expr

and operator =
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | LT of expr * expr
  | LTE of expr * expr
  | GT of expr * expr
  | GTE of expr * expr
  | EQ of expr * expr
  | And of expr * expr
  | Or of expr * expr
