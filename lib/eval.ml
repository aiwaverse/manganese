open Expr
open Either
open Val
module Ambient = Map.Make (String)

let rec eval ?(ambient : l1val Ambient.t = Ambient.empty) (e : expr) :
    (string, l1val) t =
  match e with
  | Boolean true -> Right (VBoolean true)
  | Boolean false -> Right (VBoolean false)
  | Number num -> Right (VNumber num)
  | If (cond, e1, e2) -> (
      match eval cond ~ambient with
      | Right (VBoolean true) -> eval e1 ~ambient
      | Right (VBoolean false) -> eval e2 ~ambient
      | Right e ->
          Left ("eval failed on If, expected Bool, found " ^ print_l1val e)
      | Left err -> Left ("eval failed on If\n" ^ err))
  | Var name -> (
      match Ambient.find_opt name ambient with
      | Some t -> Right t
      | None -> Left ("Variable " ^ name ^ " not found on ambient"))
  | Function (varName, _, body) -> Right (VClosure (varName, body, ambient))
  | Let (varName, _, varDefinition, body) -> (
      let value = eval varDefinition ~ambient in
      match value with
      | Right v -> eval body ~ambient:(Ambient.add varName v ambient)
      | Left err -> Left ("eval failed on Let\n" ^ err))
  | LetRec (f, _, _, x, e1, e2) -> (
      match
        eval e2
          ~ambient:(Ambient.add f (VRecClosure (f, x, e1, ambient)) ambient)
      with
      | Right v -> Right v
      | Left err -> Left ("eval failed on LetRec\n" ^ err))
  | App (e1, e2) -> (
      match eval e1 ~ambient with
      | Right (VRecClosure (f, x, e, p')) -> (
          match eval e2 ~ambient with
          | Right v' -> (
              match
                eval e
                  ~ambient:
                    (Ambient.add_seq
                       (Seq.cons (x, v')
                          (Seq.return (f, VRecClosure (f, x, e, p'))))
                       p')
              with
              | Right v -> Right v
              | Left err -> Left ("eval failed on App\n" ^ err))
          | Left err -> Left ("eval failed on App\n" ^ err))
      | Right (VClosure (x, e, p')) -> (
          match eval e2 ~ambient with
          | Right v -> (
              match eval e ~ambient:(Ambient.add x v p') with
              | Right v' -> Right v'
              | Left err -> Left ("eval failed on App\n" ^ err))
          | Left err -> Left ("eval failed on App\n" ^ err))
      | Right err ->
          Left
            ("eval failed on App, expected VClosure, found " ^ print_l1val err)
      | Left err -> Left ("evval failed on App\n" ^ err))
  | AppOp (op, e1, e2) -> (
      let v1 = eval e1 ~ambient in
      let v2 = eval e2 ~ambient in
      match v1 with
      | Right (VNumber int) -> (
          let n1 = int in
          match v2 with
          | Right (VNumber int) -> (
              let n2 = int in
              match op with
              | Add -> Right (VNumber (n1 + n2))
              | Sub -> Right (VNumber (n1 - n2))
              | Mul -> Right (VNumber (n1 * n2))
              | Div ->
                  if n2 != 0 then Right (VNumber (n1 / n2))
                  else
                    Left
                      ("eval failed on AppOp: division by zero at "
                      ^ print_expr (AppOp (op, Number n1, Number n2)))
              | LT -> Right (VBoolean (n1 < n2))
              | LTE -> Right (VBoolean (n1 <= n2))
              | GT -> Right (VBoolean (n1 > n2))
              | GTE -> Right (VBoolean (n1 >= n2))
              | EQ -> Right (VBoolean (n1 == n2))
              | _ ->
                  Left
                    ("eval failed on AppOp: " ^ print_op op
                   ^ "is not a valid operator"))
          | Right err ->
              Left
                ("eval failed on AppOp's second argument: expected a Int, found "
               ^ print_l1val err)
          | Left err -> Left ("eval failed on AppOp\n" ^ err))
      | Right (VBoolean bool) -> (
          let b1 = bool in
          match v2 with
          | Right (VBoolean bool) -> (
              let b2 = bool in
              match op with
              | And -> Right (VBoolean (b1 && b2))
              | Or -> Right (VBoolean (b1 || b2))
              | _ ->
                  Left
                    ("eval failed on AppOp: " ^ print_op op
                   ^ "is not a valid operator"))
          | Right err ->
              Left
                ("eval failed on AppOp's second argument: expected an Bool, found "
               ^ print_l1val err)
          | Left err -> Left ("eval failed on AppOp\n" ^ err))
      | Right err ->
          Left
            ("eval failed on AppOp's first argument: expected an Bool or a Int, found "
           ^ print_l1val err)
      | Left err -> Left ("eval failed on AppOp\n" ^ err))
  | Tuple (first, second) -> (
      match eval first ~ambient with
      | Right v1 -> (
          match eval second ~ambient with
          | Right v2 -> Right (VTuple (v1, v2))
          | Left err -> Left ("eval failed on Tupple\n" ^ err))
      | Left err -> Left ("eval failed on Tupple\n" ^ err))
  | Fst t -> (
      match eval t ~ambient with
      | Right (VTuple (e1, _)) -> Right e1
      | Right err ->
          Left ("eval failed on Fst: expected a Tuple, found " ^ print_l1val err)
      | Left err -> Left ("eval failed on Fst\n" ^ err))
  | Snd t -> (
      match eval t ~ambient with
      | Right (VTuple (_, e2)) -> Right e2
      | Right err ->
          Left ("eval failed on Snd: expected a Tuple, found " ^ print_l1val err)
      | Left err -> Left ("eval failed on Snd\n" ^ err))
  | Nil _ -> Right VNil
  | Cons (head, tail) -> (
      match eval head ~ambient with
      | Right vhd -> (
          match eval tail ~ambient with
          | Right vtl -> Right (VCons (vhd, vtl))
          | Left err -> Left ("eval failed on Cons\n" ^ err))
      | Left err -> Left ("eval failed on Cons\n" ^ err))
  | Head list -> (
      match eval list ~ambient with
      | Right VNil -> Left "eval failed on Head: Head of a empty list"
      | Right (VCons (hd, _)) -> Right hd
      | Right err ->
          Left ("eval failed on Head: expected a Cons, found " ^ print_l1val err)
      | Left err -> Left ("eval failed on Head\n" ^ err))
  | Tail list -> (
      match eval list ~ambient with
      | Right VNil -> Left "eval failed on Tail: Tail of a empty list"
      | Right (VCons (_, tl)) -> Right tl
      | Right err ->
          Left ("eval failed on Tail: expected a Cons, found " ^ print_l1val err)
      | Left err -> Left ("eval failed on Tail\n" ^ err))
  | MatchList (e1, e2, x, xs, e3) -> (
      match eval e1 ~ambient with
      | Right VNil -> eval e2 ~ambient
      | Right (VCons (v, vs)) ->
          eval e3
            ~ambient:
              (Ambient.add_seq (Seq.cons (x, v) (Seq.return (xs, vs))) ambient)
      | Right err ->
          Left
            ("eval failed on MatchList: expected a Cons or a Nil, found "
           ^ print_l1val err)
      | Left err -> Left ("eval failed on MatchList\n" ^ err))
  | Nothing _ -> Right VNothing
  | Just e -> (
      match eval e ~ambient with
      | Right v -> Right (VJust v)
      | Left err -> Left ("eval failed on Just\n" ^ err))
  | IsEmpty e -> (
      match eval e ~ambient with
      | Right VNil -> Right (VBoolean true)
      | Right (VCons (_, _)) -> Right (VBoolean false)
      | Right err ->
          Left
            ("eval failed on IsEmpty: expected a Cons or a Nil, found "
           ^ print_l1val err)
      | Left err -> Left ("eval failed on IsEmpty\n" ^ err))
  | IsNothing e -> (
      match eval e ~ambient with
      | Right VNothing -> Right (VBoolean true)
      | Right (VJust _) -> Right (VBoolean false)
      | Right err ->
          Left
            ("eval failed on IsNothing: expected a Nothing or a Just, found "
           ^ print_l1val err)
      | Left err -> Left ("eval failed on IsNothing\n" ^ err))
  | MatchMaybe (e1, e2, x, e3) -> (
      match eval e1 ~ambient with
      | Right VNothing -> eval e2 ~ambient
      | Right (VJust v) -> eval e3 ~ambient:(Ambient.add x v ambient)
      | Right err ->
          Left
            ("eval failed on MatchList: expected a Cons or a Nil, found "
           ^ print_l1val err)
      | Left err -> Left ("eval failed on MatchList\n" ^ err))
  | FromJust e -> (
      match eval e ~ambient with
      | Right VNothing -> Left "eval failed on FromJust: called on Nothing"
      | Right (VJust v) -> Right v
      | Right err ->
          Left
            ("eval failed on FromJust: expected a Just, found "
           ^ print_l1val err)
      | Left err -> Left ("eval failed on FromJust\n" ^ err))
