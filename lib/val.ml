module Ambient = Map.Make (String)
open Expr

type l1val =
  | VBoolean of bool
  | VNumber of int
  | VJust of l1val
  | VNothing
  | VCons of l1val * l1val
  | VNil
  | VTuple of l1val * l1val
  | VClosure of string * expr * l1val Ambient.t
  | VRecClosure of string * string * expr * l1val Ambient.t

let rec print_l1val = function
  | VBoolean b -> string_of_bool b
  | VNumber n -> string_of_int n
  | VJust v -> "just " ^ print_l1val v
  | VNothing -> "nothing"
  | VCons (h, t) -> print_l1val h ^ " :: " ^ print_l1val t
  | VNil -> "nil"
  | VTuple (v1, v2) -> "(" ^ print_l1val v1 ^ ", " ^ print_l1val v2 ^ ")"
  | VClosure (x, e, env) ->
      "<" ^ x ^ ", " ^ print_expr e ^ ", " ^ print_ambient env ^ ">"
  | VRecClosure (f, x, e, env) ->
      "<" ^ f ^ ", " ^ x ^ ", " ^ print_expr e ^ ", " ^ print_ambient env ^ ">"

and print_ambient (env : l1val Ambient.t) =
  "["
  ^ String.concat ", "
      (Ambient.fold
         (fun key value acc -> (key ^ " |-> " ^ print_l1val value) :: acc)
         env [])
  ^ "]"
