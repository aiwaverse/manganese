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

let print_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | LT -> "<"
  | LTE -> "<="
  | GT -> ">"
  | GTE -> ">="
  | EQ -> "=="
  | And -> "&&"
  | Or -> "||"

let rec print_expr = function
  | Number n -> string_of_int n
  | Boolean b -> string_of_bool b
  | If (b, e1, e2) ->
      "If " ^ print_expr b ^ " then " ^ print_expr e1 ^ " else " ^ print_expr e2
  | Function (varName, varType, body) ->
      "fn " ^ varName ^ " : " ^ print_l1Type varType ^ " => " ^ print_expr body
  | App (e1, e2) -> print_expr e1 ^ " @ " ^ print_expr e2
  | AppOp (op, e1, e2) ->
      print_expr e1 ^ " " ^ print_op op ^ " " ^ print_expr e2
  | Let (varName, varType, varDefinition, body) ->
      "Let " ^ varName ^ ":" ^ print_l1Type varType ^ " => "
      ^ print_expr varDefinition ^ " in " ^ print_expr body
  | LetRec (fName, t1, t2, innerFName, e1, e2) ->
      "Let Rec " ^ fName ^ " : " ^ print_l1Type t1 ^ " -> " ^ print_l1Type t2
      ^ " = (fn " ^ innerFName ^ " : " ^ print_l1Type t1 ^ " => "
      ^ print_expr e1 ^ ") in " ^ print_expr e2
  | Var name -> name
  | Tuple (first, second) ->
      "(" ^ print_expr first ^ ", " ^ print_expr second ^ ")"
  | Fst e -> "fst " ^ print_expr e
  | Snd e -> "snd " ^ print_expr e
  | Nil t -> "(nil : " ^ print_l1Type t ^ ")"
  | Cons (e1, e2) -> print_expr e1 ^ " :: " ^ print_expr e2
  | Head e -> "hd " ^ print_expr e
  | Tail e -> "tl " ^ print_expr e
  | MatchList (e1, e2, x, xs, e3) ->
      "match " ^ print_expr e1 ^ " with nil => " ^ print_expr e2 ^ " | " ^ x
      ^ " :: " ^ xs ^ " => " ^ print_expr e3
  | Just e -> "just " ^ print_expr e
  | Nothing t -> "nothing : " ^ print_l1Type t
  | MatchMaybe (e1, e2, x, e3) ->
      "match " ^ print_expr e1 ^ " with nil => " ^ print_expr e2 ^ " | "
      ^ "just " ^ x ^ " => " ^ print_expr e3
  | IsEmpty e -> "isEmpty " ^ print_expr e
  | IsNothing e -> "isNothing " ^ print_expr e
  | FromJust e -> "fromJust " ^ print_expr e
