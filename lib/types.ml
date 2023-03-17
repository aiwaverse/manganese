type l1Type =
  | Int
  | Bool
  | Func of l1Type * l1Type
  | List of l1Type
  | Pair of l1Type * l1Type
  | Maybe of l1Type

let rec print_l1Type = function
  | Int -> "Int"
  | Bool -> "Bool"
  | Func (t1, t2) -> print_l1Type t1 ^ " -> " ^ print_l1Type t2
  | List t -> "[" ^ print_l1Type t ^ "]"
  | Pair (t1, t2) -> "(" ^ print_l1Type t1 ^ ", " ^ print_l1Type t2 ^ ")"
  | Maybe t -> "Maybe " ^ print_l1Type t

let rec print_raw_l1Type = function
  | Int -> "Int"
  | Bool -> "Bool"
  | Func (t1, t2) ->
      "Func(" ^ print_raw_l1Type t1 ^ ", " ^ print_raw_l1Type t2 ^ ")"
  | List t -> "List(" ^ print_raw_l1Type t ^ ")"
  | Pair (t1, t2) ->
      "Pair(" ^ print_raw_l1Type t1 ^ ", " ^ print_raw_l1Type t2 ^ ")"
  | Maybe t -> "Maybe(" ^ print_raw_l1Type t ^ ")"
