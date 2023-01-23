open Expr
open Types
open Either
module Ambient = Map.Make (String)

let rec typeinfer (e : expr) (ambient : expr Ambient.t) =
  match e with
  | Number _ -> Right Int
  | Boolean _ -> Right Bool
  | If (cond, e1, e2) -> typeinferIf (If (cond, e1, e2)) ambient

and typeinferIf (e : expr) (ambient : expr Ambient.t) =
  match e with
  | If (cond, e1, e2) -> (
      match typeinfer cond ambient with
      | Right Bool -> (
          match typeinfer e1 ambient with
          | Right ee1 -> (
              match typeinfer e2 ambient with
              | Right ee2 ->
                  if ee1 = ee2 then Right ee1
                  else
                    Left
                      ("typeinfer failed on If, expected " ^ print_l1Type ee1
                     ^ ", found " ^ print_l1Type ee2)
              | Left err ->
                  Left ("typeinferIf failed on second argument\n" ^ err))
          | Left err -> Left ("typeinferInf failed on first argument\n" ^ err))
      | Right e ->
          Left ("typeinfer failed on If, expected Bool, found " ^ print_l1Type e)
      | Left err -> Left ("typeinferInf failed on condition\n" ^ err))
  | _ -> Left "typeinferIf used on something that isn't an if"
