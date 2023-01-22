type l1Type =
  | Int
  | Bool
  | Func of l1Type * l1Type
  | List of l1Type
  | Pair of l1Type * l1Type
  | Maybe of l1Type
