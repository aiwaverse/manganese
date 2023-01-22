open Types
open Option

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

module Ambient = Map.Make (String)

let rec typeinfer (e : expr) ambient =
  match e with
  | Number _ -> Some Int
  | Boolean _ -> Some Bool
  | If (cond, e1, e2)
    when typeinfer cond ambient = Some Bool
         && typeinfer e1 ambient = typeinfer e2 ambient ->
      typeinfer e1 ambient
  | Tuple (e1, e2)
    when is_some (typeinfer e1 ambient) && is_some (typeinfer e2 ambient) ->
      Some (Pair (get (typeinfer e1 ambient), get (typeinfer e2 ambient)))
