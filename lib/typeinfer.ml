open Expr
open Types
open Either
module Ambient = Map.Make (String)

let rec typeinfer (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | Number _ -> Right Int
  | Boolean _ -> Right Bool
  | If _ as e -> typeinferIf e ambient
  | Tuple _ as e -> typeinferTuple e ambient
  | Function _ as e -> typeinferFunction e ambient
  | Var _ as e -> typeinferVar e ambient
  | AppOp _ as e -> typeinferAppOp e ambient
  | _ -> Left "Not Implemented"

and typeinferIf (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | If (cond, e1, e2) -> (
      match typeinfer cond ambient with
      | Right Bool -> (
          match typeinfer e1 ambient with
          | Right ee1 -> (
              match typeinfer e2 ambient with
              | Right ee2 ->
                  if ee1 == ee2 then Right ee1
                  else
                    Left
                      ("typeinfer failed on If, e1 and e2 types are different, \
                        expected " ^ print_l1Type ee1 ^ ", found "
                     ^ print_l1Type ee2)
              | Left err ->
                  Left ("typeinferIf failed on second argument\n" ^ err))
          | Left err -> Left ("typeinferInf failed on first argument\n" ^ err))
      | Right e ->
          Left ("typeinfer failed on If, expected Bool, found " ^ print_l1Type e)
      | Left err -> Left ("typeinferInf failed on condition\n" ^ err))
  | _ -> Left "typeinferIf used on something that isn't an if"

and typeinferTuple (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | Tuple (e1, e2) -> (
      match typeinfer e1 ambient with
      | Right t1 -> (
          match typeinfer e2 ambient with
          | Right t2 -> Right (Pair (t1, t2))
          | Left err2 ->
              Left ("typeinferTuple failed on second argument\n" ^ err2))
      | Left err1 -> Left ("typeinferTuple failed on first argument\n" ^ err1))
  | _ -> Left "typeinferTuple used on something that isn't a tuple"

and typeinferVar (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | Var (name) -> (
      match Ambient.find_opt name ambient with
      | Some t -> Right t
      | None -> Left ("Variable " ^ name ^ " not found on ambient"))
  | _ -> Left "typeinferVar used on something that isn't a var"

and typeinferFunction (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | Function (varName, varType, body) -> (
      match typeinfer body (Ambient.add varName varType ambient) with
      | Right bodyType -> Right (Func (varType, bodyType))
      | Left err -> Left ("typeinfer failed on Function\n" ^ err))
  | _ -> Left "typeinferFunction used on something that isn't a function"

and typeinferAppOp (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | AppOp (op, e1, e2) -> (
      match op with
      | Add | Sub | Mul | Div -> (
          match typeinfer e1 ambient with
          | Right Int -> (
              match typeinfer e2 ambient with
              | Right Int -> Right Int
              | Right t ->
                  Left
                    ("typeinferAppOp failed on second argument, expected Int, \
                      found " ^ print_l1Type t)
              | Left err -> Left ("typeinfer failed on second argument\n" ^ err)
              )
          | Right t ->
              Left
                ("typeinferAppOp failed on first argument, expected Int, found "
               ^ print_l1Type t)
          | Left err -> Left ("typeinfer failed on first argument\n" ^ err))
      | LT | LTE | GT | GTE | EQ -> (
          match typeinfer e1 ambient with
          | Right Int -> (
              match typeinfer e2 ambient with
              | Right Int -> Right Bool
              | Right t ->
                  Left
                    ("typeinferAppOp failed on second argument, expected Int, \
                      found " ^ print_l1Type t)
              | Left err -> Left ("typeinfer failed on second argument\n" ^ err)
              )
          | Right t ->
              Left
                ("typeinferAppOp failed on first argument, expected Int, found "
               ^ print_l1Type t)
          | Left err -> Left ("typeinfer failed on first argument\n" ^ err))
      | And | Or -> (
          match typeinfer e1 ambient with
          | Right Bool -> (
              match typeinfer e2 ambient with
              | Right Bool -> Right Bool
              | Right t ->
                  Left
                    ("typeinferAppOp failed on second argument, expected Int, \
                      found " ^ print_l1Type t)
              | Left err -> Left ("typeinfer failed on second argument\n" ^ err)
              )
          | Right t ->
              Left
                ("typeinferAppOp failed on first argument, expected Int, found "
               ^ print_l1Type t)
          | Left err -> Left ("typeinfer failed on first argument\n" ^ err)))
  | _ ->
      Left "typeinferAppOp used on something that isn't an operator application"
