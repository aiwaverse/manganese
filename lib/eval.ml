open Expr
open Either
module Ambient = Map.Make (String)

let rec eval ?(ambient : expr Ambient.t = Ambient.empty) (e : expr) =
  match e with
  | Boolean true -> Right (Boolean true)
  | Boolean false -> Right (Boolean false)
  | Number num -> Right (Number num)
  | If (cond, e1, e2) -> (
      match eval cond ~ambient with
      | Right (Boolean true) -> eval e1 ~ambient
      | Right (Boolean false) -> eval e2 ~ambient
      | Right e ->
          Left ("eval failed on If, expected Bool, found " ^ print_expr e)
      | Left err -> Left ("eval failed on If\n" ^ err))
  | Var name -> (
      match Ambient.find_opt name ambient with
      | Some t -> Right t
      | None -> Left ("Variable " ^ name ^ " not found on ambient"))
  | Function (varName, varType, body) ->
      Right (Function (varName, varType, body))
  | Let (varName, _, varDefinition, body) -> (
      let value = eval varDefinition ~ambient in
      match value with
      | Right v -> eval body ~ambient:(Ambient.add varName v ambient)
      | Left err -> Left ("eval failed on Let\n" ^ err))
  | LetRec (fName, t1, t2, innerFName, e1, e2) ->
      eval e2
        ~ambient:
          (Ambient.add fName
             (Function
                (innerFName, t1, LetRec (fName, t1, t2, innerFName, e1, e1)))
             ambient)
  | App (f, arg) -> (
      match eval f ~ambient with
      | Right (Function (varName, _, body)) -> (
          match eval arg ~ambient with
          | Right t -> eval body ~ambient:(Ambient.add varName t ambient)
          | Left err -> Left ("eval failed on App\n" ^ err))
      | Right err ->
          Left
            ("eval failed on App: expected a Function, found" ^ print_expr err)
      | Left err -> Left ("eval failed on App\n" ^ err))
  | AppOp (op, e1, e2) -> (
      let v1 = eval e1 ~ambient in
      let v2 = eval e2 ~ambient in
      match v1 with
      | Right (Number int) -> (
          let n1 = int in
          match v2 with
          | Right (Number int) -> (
              let n2 = int in
              match op with
              | Add -> Right (Number (n1 + n2))
              | Sub -> Right (Number (n1 - n2))
              | Mul -> Right (Number (n1 * n2))
              | Div ->
                  if n2 != 0 then Right (Number (n1 / n2))
                  else
                    Left
                      ("eval failed on AppOp: division by zero at "
                      ^ print_expr (AppOp (op, Number n1, Number n2)))
              | LT -> Right (Boolean (n1 < n2))
              | LTE -> Right (Boolean (n1 <= n2))
              | GT -> Right (Boolean (n1 > n2))
              | GTE -> Right (Boolean (n1 >= n2))
              | EQ -> Right (Boolean (n1 == n2))
              | _ ->
                  Left
                    ("eval failed on AppOp: " ^ print_op op
                   ^ "is not a valid operator"))
          | Right err ->
              Left
                ("eval failed on AppOp's second argument: expected a Int, \
                  found " ^ print_expr err)
          | Left err -> Left ("eval failed on AppOp\n" ^ err))
      | Right (Boolean bool) -> (
          let b1 = bool in
          match v2 with
          | Right (Boolean bool) -> (
              let b2 = bool in
              match op with
              | And -> Right (Boolean (b1 && b2))
              | Or -> Right (Boolean (b1 || b2))
              | _ ->
                  Left
                    ("eval failed on AppOp: " ^ print_op op
                   ^ "is not a valid operator"))
          | Right err ->
              Left
                ("eval failed on AppOp's second argument: expected an Bool, \
                  found " ^ print_expr err)
          | Left err -> Left ("eval failed on AppOp\n" ^ err))
      | Right err ->
          Left
            ("eval failed on AppOp's first argument: expected an Bool or a \
              Int, found " ^ print_expr err)
      | Left err -> Left ("eval failed on AppOp\n" ^ err))
  | Tuple (first, second) -> (
      match eval first ~ambient with
      | Right v1 -> (
          match eval second ~ambient with
          | Right v2 -> Right (Tuple (v1, v2))
          | Left err -> Left ("eval failed on Tupple\n" ^ err))
      | Left err -> Left ("eval failed on Tupple\n" ^ err))
  | Fst t -> (
      match eval t ~ambient with
      | Right (Tuple (e1, _)) -> eval e1 ~ambient
      | Right err ->
          Left ("eval failed on Fst: expected a Tuple, found " ^ print_expr err)
      | Left err -> Left ("eval failed on Fst\n" ^ err))
  | Snd t -> (
      match eval t ~ambient with
      | Right (Tuple (_, e2)) -> eval e2 ~ambient
      | Right err ->
          Left ("eval failed on Snd: expected a Tuple, found " ^ print_expr err)
      | Left err -> Left ("eval failed on Snd\n" ^ err))
  | Nil listType -> Right (Nil listType)
  | Cons (head, tail) -> (
      match eval head ~ambient with
      | Right vhd -> (
          match eval tail ~ambient with
          | Right vtl -> Right (Cons (vhd, vtl))
          | Left err -> Left ("eval failed on Cons\n" ^ err))
      | Left err -> Left ("eval failed on Cons\n" ^ err))
  | Head list -> (
      match eval list ~ambient with
      | Right (Nil _) -> Left "eval failed on Head: Head of a empty list"
      | Right (Cons (hd, _)) -> eval hd ~ambient
      | Right err ->
          Left ("eval failed on Head: expected a Cons, found " ^ print_expr err)
      | Left err -> Left ("eval failed on Head\n" ^ err))
  | Tail list -> (
      match eval list ~ambient with
      | Right (Nil _) -> Left "eval failed on Tail: Tail of a empty list"
      | Right (Cons (_, tl)) -> eval tl ~ambient
      | Right err ->
          Left ("eval failed on Tail: expected a Cons, found " ^ print_expr err)
      | Left err -> Left ("eval failed on Tail\n" ^ err))
  | MatchList (e1, e2, x, xs, e3) -> (
      match eval e1 ~ambient with
      | Right (Nil _) -> eval e2 ~ambient
      | Right (Cons (v, vs)) ->
          eval e3
            ~ambient:
              (Ambient.add_seq (Seq.cons (x, v) (Seq.return (xs, vs))) ambient)
      | Right err ->
          Left
            ("eval failed on MatchList: expected a Cons or a Nil, found "
           ^ print_expr err)
      | Left err -> Left ("eval failed on MatchList\n" ^ err))
  | Nothing t -> Right (Nothing t)
  | Just e -> (
    match eval e ~ambient with
    | Right v -> Right (Just v)
    | Left err -> Left ("eval failed on Just\n" ^ err)
  )
  | IsEmpty e -> (
    match eval e ~ambient with
    | Right (Nil _) -> Right (Boolean true)
    | Right (Cons (_, _)) -> Right (Boolean false)
    | Right err ->
        Left
          ("eval failed on IsEmpty: expected a Cons or a Nil, found "
         ^ print_expr err)
    | Left err -> Left ("eval failed on IsEmpty\n" ^ err)
  )
  | IsNothing e -> (
    match eval e ~ambient with
    | Right (Nothing _) -> Right (Boolean true)
    | Right (Just _) -> Right (Boolean false)
    | Right err ->
        Left
          ("eval failed on IsNothing: expected a Nothing or a Just, found "
         ^ print_expr err)
    | Left err -> Left ("eval failed on IsNothing\n" ^ err)
  )
  | MatchMaybe (e1, e2, x, e3) -> (
      match eval e1 ~ambient with
      | Right (Nil _) -> eval e2 ~ambient
      | Right (Just v) -> eval e3 ~ambient:(Ambient.add x v ambient)
      | Right err ->
          Left
            ("eval failed on MatchList: expected a Cons or a Nil, found "
           ^ print_expr err)
      | Left err -> Left ("eval failed on MatchList\n" ^ err))
  | FromJust e -> (
    match eval e ~ambient with
    | Right (Nothing _) -> Left ("eval failed on FromJust: called on Nothing")
    | Right (Just v) -> eval v ~ambient
    | Right err ->
        Left
          ("eval failed on FromJust: expected a Just, found "
         ^ print_expr err)
    | Left err -> Left ("eval failed on FromJust\n" ^ err)
  )
