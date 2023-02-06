open Expr
open Types
open Either
module Ambient = Map.Make (String)

let rec typeinfer ?(ambient : l1Type Ambient.t = Ambient.empty) (e : expr) =
  match e with
  | Number _ -> Right Int
  | Boolean _ -> Right Bool
  | If _ as e -> typeinferIf e ambient
  | Tuple _ as e -> typeinferTuple e ambient
  | Function _ as e -> typeinferFunction e ambient
  | Var _ as e -> typeinferVar e ambient
  | AppOp _ as e -> typeinferAppOp e ambient
  | App _ as e -> typeinferApp e ambient
  | Let _ as e -> typeinferLet e ambient
  | LetRec _ as e -> typeinferLetRec e ambient
  | Fst _ as e -> typeinferFst e ambient
  | Snd _ as e -> typeinferSnd e ambient
  | Nil _ as e -> typeinferNil e
  | Cons _ as e -> typeinferCons e ambient
  | Head _ as e -> typeinferHead e ambient
  | Tail _ as e -> typeinferTail e ambient
  | Nothing _ as e -> typeinferNothing e
  | Just _ as e -> typeinferJust e ambient
  | IsEmpty _ as e -> typeinferIsEmpty e ambient
  | IsNothing _ as e -> typeinferIsNothing e ambient
  | MatchList _ as e -> typeinferMatchList e ambient
  | MatchMaybe _ as e -> typeinferMatchMaybe e ambient
  | FromJust _ as e -> typeinferFromJust e ambient

and typeinferIf (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | If (cond, e1, e2) -> (
      match typeinfer cond ~ambient with
      | Right Bool -> (
          match typeinfer e1 ~ambient with
          | Right ee1 -> (
              match typeinfer e2 ~ambient with
              | Right ee2 ->
                  if ee1 = ee2 then Right ee1
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
      match typeinfer e1 ~ambient with
      | Right t1 -> (
          match typeinfer e2 ~ambient with
          | Right t2 -> Right (Pair (t1, t2))
          | Left err2 ->
              Left ("typeinferTuple failed on second argument\n" ^ err2))
      | Left err1 -> Left ("typeinferTuple failed on first argument\n" ^ err1))
  | _ -> Left "typeinferTuple used on something that isn't a tuple"

and typeinferVar (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | Var name -> (
      match Ambient.find_opt name ambient with
      | Some t -> Right t
      | None -> Left ("Variable " ^ name ^ " not found on ambient"))
  | _ -> Left "typeinferVar used on something that isn't a var"

and typeinferFunction (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | Function (varName, varType, body) -> (
      match typeinfer body ~ambient:(Ambient.add varName varType ambient) with
      | Right bodyType -> Right (Func (varType, bodyType))
      | Left err -> Left ("typeinfer failed on Function\n" ^ err))
  | _ -> Left "typeinferFunction used on something that isn't a function"

and typeinferAppOp (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | AppOp (op, e1, e2) -> (
      match op with
      | Add | Sub | Mul | Div | LT | LTE | GT | GTE | EQ -> (
          match typeinfer e1 ~ambient with
          | Right Int -> (
              match typeinfer e2 ~ambient with
              | Right Int ->
                  Right
                    (if List.mem op [ Add; Sub; Mul; Div ] then Int else Bool)
              | Right t ->
                  Left
                    ("typeinferAppOp failed on second argument, expected Int, \
                      found " ^ print_l1Type t)
              | Left err ->
                  Left ("typeinfer failed on AppOp's second argument\n" ^ err))
          | Right t ->
              Left
                ("typeinferAppOp failed on first argument, expected Int, found "
               ^ print_l1Type t)
          | Left err ->
              Left ("typeinfer failed on AppOp's first argument\n" ^ err))
      | And | Or -> (
          match typeinfer e1 ~ambient with
          | Right Bool -> (
              match typeinfer e2 ~ambient with
              | Right Bool -> Right Bool
              | Right t ->
                  Left
                    ("typeinferAppOp failed on second argument, expected Int, \
                      found " ^ print_l1Type t)
              | Left err ->
                  Left ("typeinfer failed on AppOp's second argument\n" ^ err))
          | Right t ->
              Left
                ("typeinferAppOp failed on first argument, expected Int, found "
               ^ print_l1Type t)
          | Left err ->
              Left ("typeinfer failed on AppOp's first argument\n" ^ err)))
  | _ ->
      Left "typeinferAppOp used on something that isn't an operator application"

and typeinferApp (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | App (f, arg) -> (
      match typeinfer f ~ambient with
      | Right (Func (t1, t2)) -> (
          match typeinfer arg ~ambient with
          | Right t when t = t1 -> Right t2
          | Right tErr ->
              Left
                ("typeinferApp failed on second argument, expected "
               ^ print_l1Type t1 ^ ", found " ^ print_l1Type tErr)
          | Left err ->
              Left ("typeinfer failed on App's second argument\n" ^ err))
      | Right t ->
          Left
            ("typeinferApp failed on first argument, expected T1 -> T2, found "
           ^ print_l1Type t)
      | Left err -> Left ("typeinfer failed on App's first argument\n" ^ err))
  | _ -> Left "typeinferApp used on something that isn't an application"

and typeinferFst (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | Fst t -> (
      match typeinfer t ~ambient with
      | Right (Pair (t1, _)) -> Right t1
      | Right t ->
          Left
            ("typeinferFst failed on first argument, expected (T1, T2), found "
           ^ print_l1Type t)
      | Left err -> Left ("typeinfer failed on Fst's first argument\n" ^ err))
  | _ -> Left "typeinferFst used on something that isn't a first"

and typeinferSnd (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | Snd t -> (
      match typeinfer t ~ambient with
      | Right (Pair (_, t2)) -> Right t2
      | Right t ->
          Left
            ("typeinferSnd failed on first argument, expected (T1, T2), found "
           ^ print_l1Type t)
      | Left err -> Left ("typeinfer failed on Snd's first argument\n" ^ err))
  | _ -> Left "typeinferSnd used on something that isn't a second"

and typeinferLet (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | Let (varName, varType, varDefinition, body) -> (
      match typeinfer varDefinition ~ambient with
      | Right t when t = varType ->
          typeinfer body ~ambient:(Ambient.add varName varType ambient)
      | Right tErr ->
          Left
            ("typeinfer failed on let, type of " ^ varName ^ " is defined as "
           ^ print_l1Type varType ^ ", but inferred as " ^ print_l1Type tErr)
      | Left err -> Left err)
  | _ -> Left "typeinferLet used on something that isn't a let"

and typeinferLetRec (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | LetRec (fName, t1, t2, innerFName, e1, e2) -> (
      let gamma' = Ambient.add fName (Func (t1, t2)) ambient in
      match typeinfer e1 ~ambient:(Ambient.add innerFName t1 gamma') with
      | Right t ->
          if t = t2 then typeinfer e2 ~ambient:gamma'
          else
            Left
              ("typeinfer failed on let rec, expected type " ^ print_l1Type t2
             ^ ", found " ^ print_l1Type t)
      | Left err -> Left ("typeinfer failed on let rec.\n" ^ err))
  | _ -> Left "typeinferLetRec used on something that isn't a let rec"

and typeinferNil (e : expr) =
  match e with
  | Nil t -> Right (List t)
  | _ -> Left "typeinferNil used on something that isn't a Nil"

and typeinferCons (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | Cons (x, xs) -> (
      match typeinfer x ~ambient with
      | Right t -> (
          match typeinfer xs ~ambient with
          | Right (List xt) ->
              if t = xt then Right (List t)
              else
                Left
                  ("typeinfer failed on Cons, expected type " ^ print_l1Type xt
                 ^ ", but found " ^ print_l1Type t)
          | Right tErr ->
              Left
                ("typeinfer failed on Cons, expected xs to be a List, but \
                  found " ^ print_l1Type tErr)
          | Left err -> Left ("typeinfer failed on Cons\n" ^ err))
      | Left err -> Left ("typeinfer failed on Cons\n" ^ err))
  | _ -> Left "typeinferCons used on something that isn't a cons"

and typeinferNothing (e : expr) =
  match e with
  | Nothing t -> Right (Maybe t)
  | _ -> Left "typeinferNothing used on something that isn't a Nothing"

and typeinferJust (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | Just exp -> (
      match typeinfer exp ~ambient with
      | Right t -> Right (Maybe t)
      | Left err -> Left ("typeinfer failed on Just\n" ^ err))
  | _ -> Left "typeinferJust used on something that isn't a Just"

and typeinferIsEmpty (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | IsEmpty ee -> (
      match typeinfer ee ~ambient with
      | Right (List _) -> Right Bool
      | Right t ->
          Left
            ("typeinfer failed on IsEmpty, expected a List, found "
           ^ print_l1Type t)
      | Left err -> Left ("typeinfer failed on IsEmpty\n" ^ err))
  | _ -> Left "typeinferIsEmpty used on something that isn't an IsEmpty"

and typeinferIsNothing (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | IsNothing ee -> (
      match typeinfer ee ~ambient with
      | Right (Maybe _) -> Right Bool
      | Right t ->
          Left
            ("typeinfer failed on IsNothing, expected a Maybe, found "
           ^ print_l1Type t)
      | Left err -> Left ("typeinfer failed on IsNothing\n" ^ err))
  | _ -> Left "typeinferIsNothing used on something that isn't an IsNothing"

and typeinferFromJust (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | FromJust ee -> (
      match typeinfer ee ~ambient with
      | Right (Maybe t) -> Right t
      | Right t ->
          Left
            ("typeinfer failed on FromJust, expected a Maybe, found "
           ^ print_l1Type t)
      | Left err -> Left ("typeinfer failed on FromJust\n" ^ err))
  | _ -> Left "typeinferFromJust used on something that isn't an FromJust"

and typeinferMatchList (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | MatchList (e1, e2, x, xs, e3) -> (
      match typeinfer e1 ~ambient with
      | Right (List t) -> (
          match typeinfer e2 ~ambient with
          | Right t2 -> (
              match
                typeinfer e3
                  ~ambient:
                    (Ambient.add_seq
                       (Seq.cons (x, t) (Seq.return (xs, List t)))
                       ambient)
              with
              | Right t3 when t2 = t3 -> Right t2
              | Right tErr ->
                  Left
                    ("typeinfer failed on MatchList, e2 and e3 types don't \
                      match, e2 is " ^ print_l1Type t2 ^ " and e3 is "
                   ^ print_l1Type tErr)
              | Left err ->
                  Left
                    ("typeinfer failed on MatchList, e3 typeinfer failed with\n"
                   ^ err))
          | Left err ->
              Left
                ("typeinfer failed on MatchList, e2 typeinfer failed with\n"
               ^ err))
      | Right t ->
          Left
            ("typeinfer failed on MatchList, expected e1 to be a List, found "
           ^ print_l1Type t)
      | Left err -> Left ("typeinfer failed on MatchList\n" ^ err))
  | _ -> Left "typeinferMatchList used on something that isn't a MatchList"

and typeinferMatchMaybe (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | MatchMaybe (e1, e2, x, e3) -> (
      match typeinfer e1 ~ambient with
      | Right (Maybe t) -> (
          match typeinfer e2 ~ambient with
          | Right t2 -> (
              match typeinfer e3 ~ambient:(Ambient.add x t ambient) with
              | Right t3 when t2 = t3 -> Right t2
              | Right tErr ->
                  Left
                    ("typeinfer failed on MatchMaybe, e2 and e3 types don't \
                      match, e2 is " ^ print_l1Type t2 ^ " and e3 is "
                   ^ print_l1Type tErr)
              | Left err ->
                  Left
                    ("typeinfer failed on MatchMaybe, e3 typeinfer failed with\n"
                   ^ err))
          | Left err ->
              Left
                ("typeinfer failed on MatchMaybe, e2 typeinfer failed with\n"
               ^ err))
      | Right t ->
          Left
            ("typeinfer failed on MatchMaybe, expected e1 to be a List, found "
           ^ print_l1Type t)
      | Left err -> Left ("typeinfer failed on MatchMaybe\n" ^ err))
  | _ -> Left "typeinferMatchMaybe used on something that isn't a MatchMaybe"

and typeinferHead (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | Head xs -> (
      match typeinfer xs ~ambient with
      | Right (List t) -> Right t
      | Right t ->
          Left
            ("typeinfer failed on Head, expected a list, found "
           ^ print_l1Type t)
      | Left err -> Left ("typeinfer failed on Head\n" ^ err))
  | _ -> Left "typeinferHead used on something that isn't a Head"

and typeinferTail (e : expr) (ambient : l1Type Ambient.t) =
  match e with
  | Tail xs -> (
      match typeinfer xs ~ambient with
      | Right (List _ as t) -> Right t
      | Right t ->
          Left
            ("typeinfer failed on Tail, expected a list, found "
           ^ print_l1Type t)
      | Left err -> Left ("typeinfer failed on Tail\n" ^ err))
  | _ -> Left "typeinferTail used on something that isn't a Tail"
