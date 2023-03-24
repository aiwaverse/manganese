open Manganese.Expr
open Manganese.Eval
open Manganese.Val
open Manganese.Types
open Either
open OUnit2

let simpleTests =
  [
    ( "simple number" >:: fun _ ->
      assert_equal (Right (VNumber 2)) (eval (Number 2)) );
    ( "simple boolean - true" >:: fun _ ->
      assert_equal (Right (VBoolean true)) (eval (Boolean true)) );
    ( "simple boolean - false" >:: fun _ ->
      assert_equal (Right (VBoolean false)) (eval (Boolean false)) );
    ( "function" >:: fun _ ->
      assert_equal
        (Right
           (VClosure
              ("x", If (Var "x", Boolean true, Boolean false), Ambient.empty)))
        (eval (Function ("x", Bool, If (Var "x", Boolean true, Boolean false))))
    );
    ( "let rec" >:: fun _ ->
      assert_equal (Right (VNumber 120))
        (eval
           (LetRec
              ( "fat",
                Int,
                Int,
                "x",
                If
                  ( AppOp (EQ, Var "x", Number 0),
                    Number 1,
                    AppOp
                      ( Mul,
                        Var "x",
                        App (Var "fat", AppOp (Sub, Var "x", Number 1)) ) ),
                App (Var "fat", Number 5) ))) );
  ]

let ifTests =
  [
    ( "if - well written" >:: fun _ ->
      assert_equal (Right (VBoolean true))
        (eval (If (Boolean true, Boolean true, Boolean false))) );
    ( "if - poorly written, cond isn't boolean" >:: fun _ ->
      assert_bool "Failed for if"
        (is_left (eval (If (Number 0, Boolean true, Boolean false)))) );
    ( "if - poorly written, cond poorly written" >:: fun _ ->
      assert_bool "Failed for if"
        (is_left
           (eval
              (If
                 ( If (Number 0, Boolean true, Boolean false),
                   Boolean true,
                   Boolean false )))) );
  ]

let tupleTests =
  [
    ( "tuple - well written" >:: fun _ ->
      assert_equal
        (Right (VTuple (VNumber 0, VBoolean true)))
        (eval (Tuple (Number 0, Boolean true))) );
    ( "tuple - poorly written, first term is poorly written" >:: fun _ ->
      assert_bool "Failed for tuple"
        (is_left
           (eval (Tuple (If (Number 2, Number 1, Boolean false), Number 2)))) );
    ( "tuple - poorly written, second term is poorly written" >:: fun _ ->
      assert_bool "Failed for tuple"
        (is_left
           (eval (Tuple (Number 2, If (Number 2, Number 1, Boolean false))))) );
  ]

let varTests =
  [
    ( "var - in ambient" >:: fun _ ->
      assert_equal (Right (VNumber 0))
        (eval (Var "x") ~ambient:(Ambient.add "x" (VNumber 0) Ambient.empty)) );
    ( "var - not in ambient" >:: fun _ ->
      assert_bool "Failed for var"
        (is_left
           (eval (Var "x") ~ambient:(Ambient.add "y" (VNumber 0) Ambient.empty)))
    );
  ]

let appTests =
  [
    ( "application - well written" >:: fun _ ->
      assert_equal (Right (VBoolean true))
        (eval
           (App
              ( Function ("x", Bool, If (Var "x", Boolean true, Boolean false)),
                Boolean true ))
           ~ambient:Ambient.empty) );
    ( "application - poorly written, first term isn't a function" >:: fun _ ->
      assert_bool "failed on application"
        (is_left (eval (App (Boolean false, Boolean true)))) );
  ]

let appOpTests =
  [
    ( "operator application, add - well written" >:: fun _ ->
      assert_equal (Right (VNumber 3)) (eval (AppOp (Add, Number 1, Number 2)))
    );
    ( "operator application, add - poorly written, first expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (Add, Boolean false, Number 2)))) );
    ( "operator application, add - poorly written, second expression isn't \
       number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (Add, Number 1, Boolean false)))) );
    ( "operator application, sub - well written" >:: fun _ ->
      assert_equal (Right (VNumber (-1)))
        (eval (AppOp (Sub, Number 1, Number 2))) );
    ( "operator application, sub - poorly written, first expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (Sub, Boolean false, Number 2)))) );
    ( "operator application, sub - poorly written, second expression isn't \
       number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (Sub, Number 1, Boolean false)))) );
    ( "operator application, mul - well written" >:: fun _ ->
      assert_equal (Right (VNumber 2)) (eval (AppOp (Mul, Number 1, Number 2)))
    );
    ( "operator application, mul - poorly written, first expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (Mul, Boolean false, Number 2)))) );
    ( "operator application, mul - poorly written, second expression isn't \
       number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (Mul, Number 1, Boolean false)))) );
    ( "operator application, div - well written" >:: fun _ ->
      assert_equal (Right (VNumber 0)) (eval (AppOp (Div, Number 1, Number 2)))
    );
    ( "operator application, div - poorly written, first expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (Div, Boolean false, Number 2)))) );
    ( "operator application, div - poorly written, second expression isn't \
       number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (Div, Number 1, Boolean false)))) );
    ( "operator application, div - poorly written, second expression evaluates \
       to zero"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (Div, Number 1, Number 0)))) );
    ( "operator application, LT - well written" >:: fun _ ->
      assert_equal (Right (VBoolean true))
        (eval (AppOp (LT, Number 1, Number 2))) );
    ( "operator application, LT - poorly written, first expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (LT, Boolean false, Number 2)))) );
    ( "operator application, LT - poorly written, second expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (LT, Number 1, Boolean false)))) );
    ( "operator application, LTE - well written" >:: fun _ ->
      assert_equal (Right (VBoolean true))
        (eval (AppOp (LTE, Number 1, Number 2))) );
    ( "operator application, LTE - poorly written, first expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (LTE, Boolean false, Number 2)))) );
    ( "operator application, LTE - poorly written, second expression isn't \
       number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (LTE, Number 1, Boolean false)))) );
    ( "operator application, GT - well written" >:: fun _ ->
      assert_equal (Right (VBoolean false))
        (eval (AppOp (GT, Number 1, Number 2))) );
    ( "operator application, GT - poorly written, first expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (GT, Boolean false, Number 2)))) );
    ( "operator application, GT - poorly written, second expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (GT, Number 1, Boolean false)))) );
    ( "operator application, GTE - well written" >:: fun _ ->
      assert_equal (Right (VBoolean false))
        (eval (AppOp (GTE, Number 1, Number 2))) );
    ( "operator application, GTE - poorly written, first expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (GTE, Boolean false, Number 2)))) );
    ( "operator application, GTE - poorly written, second expression isn't \
       number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (GTE, Number 1, Boolean false)))) );
    ( "operator application, EQ - well written" >:: fun _ ->
      assert_equal (Right (VBoolean false))
        (eval (AppOp (EQ, Number 1, Number 2))) );
    ( "operator application, EQ - poorly written, first expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (EQ, Boolean false, Number 2)))) );
    ( "operator application, EQ - poorly written, second expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (EQ, Number 1, Boolean false)))) );
    ( "operator application, And - well written" >:: fun _ ->
      assert_equal (Right (VBoolean true))
        (eval (AppOp (And, Boolean true, Boolean true))) );
    ( "operator application, And - poorly written, first expression isn't \
       boolean"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (And, Number 1, Boolean false)))) );
    ( "operator application, And - poorly written, second expression isn't \
       number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (And, Boolean false, Number 1)))) );
    ( "operator application, Or - well written" >:: fun _ ->
      assert_equal (Right (VBoolean true))
        (eval (AppOp (Or, Boolean true, Boolean false))) );
    ( "operator application, Or - poorly written, first expression isn't boolean"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (Or, Number 1, Boolean false)))) );
    ( "operator application, Or - poorly written, second expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (eval (AppOp (Or, Boolean false, Number 1)))) );
  ]

let letTests =
  [
    ( "let - well written" >:: fun _ ->
      assert_equal (Right (VNumber 3))
        (eval (Let ("x", Int, Number 1, AppOp (Add, Var "x", Number 2)))) );
    ( "let - poorly written, body poorly written" >:: fun _ ->
      assert_bool "failed on let"
        (is_left
           (eval (Let ("x", Int, Number 1, AppOp (Add, Var "y", Number 2))))) );
  ]

let fstTests =
  [
    ( "fst - well written" >:: fun _ ->
      assert_equal (Right (VNumber 1))
        (eval (Fst (Tuple (Number 1, Boolean false)))) );
    ( "fst - poorly written" >:: fun _ ->
      assert_bool "failed on fst"
        (is_left (eval (Fst (Tuple (Var "x", Boolean false))))) );
  ]

let sndTests =
  [
    ( "snd - well written" >:: fun _ ->
      assert_equal (Right (VBoolean false))
        (eval (Snd (Tuple (Number 1, Boolean false)))) );
    ( "snd - poorly written" >:: fun _ ->
      assert_bool "failed on snd"
        (is_left (eval (Snd (Tuple (Var "x", Boolean false))))) );
  ]

let nilTests =
  [
    ( "nil - well written" >:: fun _ ->
      assert_equal (Right VNil) (eval (Nil Int)) );
  ]

let consTests =
  [
    ( "cons - well written" >:: fun _ ->
      assert_equal
        (Right (VCons (VNumber 1, VNil)))
        (eval (Cons (Number 1, Nil Int))) );
    ( "cons - poorly written" >:: fun _ ->
      assert_bool "failed on cons" (is_left (eval (Cons (Var "x", Nil Int)))) );
  ]

let headTests =
  [
    ( "head - well typed" >:: fun _ ->
      assert_equal (Right (VNumber 1)) (eval (Head (Cons (Number 1, Nil Int))))
    );
    ( "head - poorly typed, argument is an empty list" >:: fun _ ->
      assert_bool "failed on head" (is_left (eval (Head (Nil Int)))) );
    ( "head - poorly typed, argument isn't a list" >:: fun _ ->
      assert_bool "failed on head" (is_left (eval (Head (Number 1)))) );
    ( "head - poorly typed, argument is poorly typed" >:: fun _ ->
      assert_bool "failed on head"
        (is_left (eval (Head (Cons (Var "x", Nil Int))))) );
  ]

let tailTests =
  [
    ( "tail - well typed" >:: fun _ ->
      assert_equal (Right VNil) (eval (Tail (Cons (Number 1, Nil Int)))) );
    ( "tail - poorly typed, argument is an empty list" >:: fun _ ->
      assert_bool "failed on tail" (is_left (eval (Tail (Nil Int)))) );
    ( "tail - poorly typed, argument isn't a list" >:: fun _ ->
      assert_bool "failed on tail" (is_left (eval (Tail (Number 1)))) );
    ( "tail - poorly typed, argument is poorly typed" >:: fun _ ->
      assert_bool "failed on tail"
        (is_left (eval (Tail (Cons (Var "x", Nil Int))))) );
  ]

let tests =
  "test suite for eval"
  >::: simpleTests @ ifTests @ tupleTests @ varTests @ appTests @ appOpTests
       @ letTests @ fstTests @ sndTests @ nilTests @ consTests @ headTests
       @ tailTests
(*@ nothingTests @ justTests
  @ matchListTests @ matchMaybeTests @ isEmptyTests @ isNothingTests
  @ fromJustTests*)

let _ = run_test_tt_main tests
