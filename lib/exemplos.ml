open Expr
open Types

let test1string =
  "let x : int = 2 in (let foo : int -> int = fn y : int => x + y in let x : int = 5 in foo 10)"

let test1 =
  Let
    ( "x",
      Int,
      Number 2,
      Let
        ( "foo",
          Func (Int, Int),
          Function ("y", Int, AppOp (Add, Var "x", Var "y")),
          Let ("x", Int, Number 5, App (Var "foo", Number 10)) ) )

let test2string =
  "let x : int = 2 in (let foo : int -> int = fn y : int => x + y in let x : int = 5 in foo)"

let test2 =
  Let
    ( "x",
      Int,
      Number 2,
      Let
        ( "foo",
          Func (Int, Int),
          Function ("y", Int, AppOp (Add, Var "x", Var "y")),
          Let ("x", Int, Number 5, Var "foo") ) )

let test3string =
  "let rec lookup (l: [(int, int)]): int -> maybe int = (fn key : int => match l with nil => nothing : int | x :: xs => if (fst x) = key then just (snd x) else (lookup xs) key) in (lookup [(1,10),(2,20), (3,30)] : [(int, int)]) 2"

let test3 =
  LetRec
    ( "lookup",
      List (Pair (Int, Int)),
      Func (Int, Maybe Int),
      "l",
      Function
        ( "key",
          Int,
          MatchList
            ( Var "l",
              Nothing Int,
              "x",
              "xs",
              If
                ( AppOp (EQ, Fst (Var "x"), Var "key"),
                  Just (Snd (Var "x")),
                  App (App (Var "lookup", Var "xs"), Var "key") ) ) ),
      App
        ( App
            ( Var "lookup",
              Cons
                ( Tuple (Number 1, Number 10),
                  Cons
                    ( Tuple (Number 2, Number 20),
                      Cons (Tuple (Number 3, Number 30), Nil (Pair (Int, Int)))
                    ) ) ),
          Number 2 ) )

let test4string =
  "let rec map (f : int -> int): [int] -> [int] = (fn l : [int] => match l with nil => nil : int | x :: xs => (f x) :: ((map f) xs)) in (map (fn x: int => x + x)) [10, 20, 30] : [int]"

let test4 =
  LetRec
    ( "map",
      Func (Int, Int),
      Func (List Int, List Int),
      "f",
      Function
        ( "l",
          List Int,
          MatchList
            ( Var "l",
              Nil Int,
              "x",
              "xs",
              Cons
                ( App (Var "f", Var "x"),
                  App (App (Var "map", Var "f"), Var "xs") ) ) ),
      App
        ( App (Var "map", Function ("x", Int, AppOp (Add, Var "x", Var "x"))),
          Cons (Number 10, Cons (Number 20, Cons (Number 30, Nil Int))) ) )
