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

let print_raw_op = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | LT -> "LT"
  | LTE -> "LTE"
  | GT -> "GT"
  | GTE -> "GTE"
  | EQ -> "EQ"
  | And -> "And"
  | Or -> "Or"

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
      "Let " ^ varName ^ " : " ^ print_l1Type varType ^ " => "
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

let rec print_raw_expr = function
  | Number i -> "Number(" ^ string_of_int i ^ ")"
  | Boolean b -> "Boolean(" ^ string_of_bool b ^ ")"
  | If (b, e1, e2) ->
      "If(" ^ print_raw_expr b ^ ", " ^ print_raw_expr e1 ^ ", "
      ^ print_raw_expr e2 ^ ")"
  | Function (x, t, e) ->
      "Function(" ^ x ^ ", " ^ print_raw_l1Type t ^ ", " ^ print_raw_expr e
      ^ ")"
  | App (e1, e2) -> "App(" ^ print_raw_expr e1 ^ ", " ^ print_raw_expr e2 ^ ")"
  | AppOp (op, e1, e2) ->
      "AppOp(" ^ print_raw_op op ^ ", " ^ print_raw_expr e1 ^ ", "
      ^ print_raw_expr e2 ^ ")"
  | Let (x, t, e1, e2) ->
      "Let(" ^ x ^ ", " ^ print_raw_l1Type t ^ ", " ^ print_raw_expr e1 ^ ", "
      ^ print_raw_expr e2 ^ ")"
  | LetRec (x, t1, t2, f, e1, e2) ->
      "LetRec(" ^ x ^ ", " ^ print_raw_l1Type t1 ^ ", " ^ print_raw_l1Type t2
      ^ ", " ^ f ^ ", " ^ print_raw_expr e1 ^ ", " ^ print_raw_expr e2 ^ ")"
  | Var s -> "Var(" ^ s ^ ")"
  | Tuple (e1, e2) ->
      "Tuple(" ^ print_raw_expr e1 ^ ", " ^ print_raw_expr e2 ^ ")"
  | Fst e -> "Fst(" ^ print_raw_expr e ^ ")"
  | Snd e -> "Snd(" ^ print_raw_expr e ^ ")"
  | Nil t -> "Nil(" ^ print_raw_l1Type t ^ ")"
  | Cons (h, t) -> "Cons(" ^ print_raw_expr h ^ ", " ^ print_raw_expr t ^ ")"
  | Head e -> "Head(" ^ print_raw_expr e ^ ")"
  | Tail e -> "Tail(" ^ print_raw_expr e ^ ")"
  | MatchList (e1, e2, x, xs, e3) ->
      "MatchList(" ^ print_raw_expr e1 ^ ", " ^ print_raw_expr e2 ^ ", " ^ x
      ^ ", " ^ xs ^ ", " ^ print_raw_expr e3 ^ ")"
  | Just e -> "Just(" ^ print_raw_expr e ^ ")"
  | Nothing t -> "Nothing(" ^ print_raw_l1Type t ^ ")"
  | MatchMaybe (e1, e2, x, e3) ->
      "MatchMaybe(" ^ print_raw_expr e1 ^ ", " ^ print_raw_expr e2 ^ ", " ^ x
      ^ ", " ^ print_raw_expr e3 ^ ")"
  | IsEmpty e -> "IsEmpty(" ^ print_raw_expr e ^ ")"
  | IsNothing e -> "IsNothing(" ^ print_raw_expr e ^ ")"
  | FromJust e -> "FromJust(" ^ print_raw_expr e ^ ")"
