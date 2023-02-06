open Types

type expr =
  | Number of int
  | Boolean of bool
  | If of expr * expr * expr
  | Function of string * l1Type * expr
  | App of expr * expr
  | AppOp of operator * expr * expr
  | Let of string * l1Type * expr * expr
  | LetRec of string * l1Type * l1Type * string * expr * expr
  | Var of string
  | Tuple of expr * expr
  | Fst of expr
  | Snd of expr
  | Nil of l1Type
  | Cons of expr * expr
  | Head of expr
  | Tail of expr
  | MatchList of expr * expr * string * string * expr
  | Just of expr
  | Nothing of l1Type
  | MatchMaybe of expr * expr * string * expr
  | IsEmpty of expr
  | IsNothing of expr
  | FromJust of expr

and operator = Add | Sub | Mul | Div | LT | LTE | GT | GTE | EQ | And | Or
