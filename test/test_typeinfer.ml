open Manganese.Expr
open Manganese.Typeinfer
open Manganese.Types
open Either
open OUnit2

let simpleTests =
  [
    ( "simple number" >:: fun _ ->
      assert_equal (Right Int) (typeinfer (Number 2)) );
    ( "simple boolean - true" >:: fun _ ->
      assert_equal (Right Bool) (typeinfer (Boolean true)) );
    ( "simple boolean - false" >:: fun _ ->
      assert_equal (Right Bool) (typeinfer (Boolean false)) );
  ]

let ifTests =
  [
    ( "if - well typed" >:: fun _ ->
      assert_equal (Right Bool)
        (typeinfer (If (Boolean true, Boolean true, Boolean false))) );
    ( "if - poorly typed, cond isn't boolean" >:: fun _ ->
      assert_bool "Failed for if"
        (is_left (typeinfer (If (Number 0, Boolean true, Boolean false)))) );
    ( "if - poorly typed, e1 isn't the same type as e2" >:: fun _ ->
      assert_bool "Failed for if"
        (is_left (typeinfer (If (Boolean true, Number 1, Boolean false)))) );
    ( "if - poorly typed, cond isn't boolean and e1 isn't the same type as e2"
    >:: fun _ ->
      assert_bool "Failed for if"
        (is_left (typeinfer (If (Number 3, Number 1, Boolean false)))) );
  ]

let tupleTests =
  [
    ( "tuple - well typed" >:: fun _ ->
      assert_equal
        (Right (Pair (Int, Bool)))
        (typeinfer (Tuple (Number 0, Boolean true))) );
    ( "tuple - poorly typed, first term is poorly typed" >:: fun _ ->
      assert_bool "Failed for tuple"
        (is_left
           (typeinfer
              (Tuple (If (Boolean true, Number 1, Boolean false), Number 2))))
    );
    ( "tuple - poorly typed, second term is poorly typed" >:: fun _ ->
      assert_bool "Failed for tuple"
        (is_left
           (typeinfer
              (Tuple (Number 2, If (Boolean true, Number 1, Boolean false)))))
    );
  ]

let varTests =
  [
    ( "var - in ambient" >:: fun _ ->
      assert_equal (Right Int)
        (typeinfer (Var "x") ~ambient:(Ambient.add "x" Int Ambient.empty)) );
    ( "var - not in ambient" >:: fun _ ->
      assert_bool "Failed for var"
        (is_left
           (typeinfer (Var "x") ~ambient:(Ambient.add "y" Int Ambient.empty)))
    );
  ]

let functionTests =
  [
    ( "function - well typed" >:: fun _ ->
      assert_equal
        (Right (Func (Bool, Bool)))
        (typeinfer
           (Function ("x", Bool, If (Var "x", Boolean true, Boolean false)))) );
    ( "function - body poorly typed" >:: fun _ ->
      assert_bool "failed on function"
        (is_left
           (typeinfer
              (Function ("x", Bool, If (Var "x", Number 3, Boolean false))))) );
  ]

let appTests =
  [
    ( "application - well typed" >:: fun _ ->
      assert_equal (Right Bool)
        (typeinfer
           (App
              ( Function ("x", Bool, If (Var "x", Boolean true, Boolean false)),
                Boolean true ))) );
    ( "application - poorly typed, first term isn't a function" >:: fun _ ->
      assert_bool "failed on application"
        (is_left (typeinfer (App (Boolean false, Boolean true)))) );
    ( "application - poorly typed, second term isn't of correct type"
    >:: fun _ ->
      assert_bool "failed on application"
        (is_left
           (typeinfer
              (App
                 ( Function
                     ("x", Bool, If (Var "x", Boolean true, Boolean false)),
                   Number 0 )))) );
  ]

let appOpTests =
  [
    ( "operator application, add - well typed" >:: fun _ ->
      assert_equal (Right Int) (typeinfer (AppOp (Add, Number 1, Number 2))) );
    ( "operator application, add - poorly typed, first expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (typeinfer (AppOp (Add, Boolean false, Number 2)))) );
    ( "operator application, add - poorly typed, second expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (typeinfer (AppOp (Add, Number 1, Boolean false)))) );
    ( "operator application, sub - well typed" >:: fun _ ->
      assert_equal (Right Int) (typeinfer (AppOp (Sub, Number 1, Number 2))) );
    ( "operator application, sub - poorly typed, first expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (typeinfer (AppOp (Sub, Boolean false, Number 2)))) );
    ( "operator application, sub - poorly typed, second expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (typeinfer (AppOp (Sub, Number 1, Boolean false)))) );
    ( "operator application, mul - well typed" >:: fun _ ->
      assert_equal (Right Int) (typeinfer (AppOp (Mul, Number 1, Number 2))) );
    ( "operator application, mul - poorly typed, first expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (typeinfer (AppOp (Mul, Boolean false, Number 2)))) );
    ( "operator application, mul - poorly typed, second expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (typeinfer (AppOp (Mul, Number 1, Boolean false)))) );
    ( "operator application, div - well typed" >:: fun _ ->
      assert_equal (Right Int) (typeinfer (AppOp (Div, Number 1, Number 2))) );
    ( "operator application, div - poorly typed, first expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (typeinfer (AppOp (Div, Boolean false, Number 2)))) );
    ( "operator application, div - poorly typed, second expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (typeinfer (AppOp (Div, Number 1, Boolean false)))) );
    ( "operator application, LT - well typed" >:: fun _ ->
      assert_equal (Right Bool) (typeinfer (AppOp (LT, Number 1, Number 2))) );
    ( "operator application, LT - poorly typed, first expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (typeinfer (AppOp (LT, Boolean false, Number 2)))) );
    ( "operator application, LT - poorly typed, second expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (typeinfer (AppOp (LT, Number 1, Boolean false)))) );
    ( "operator application, LTE - well typed" >:: fun _ ->
      assert_equal (Right Bool) (typeinfer (AppOp (LTE, Number 1, Number 2))) );
    ( "operator application, LTE - poorly typed, first expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (typeinfer (AppOp (LTE, Boolean false, Number 2)))) );
    ( "operator application, LTE - poorly typed, second expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (typeinfer (AppOp (LTE, Number 1, Boolean false)))) );
    ( "operator application, GT - well typed" >:: fun _ ->
      assert_equal (Right Bool) (typeinfer (AppOp (GT, Number 1, Number 2))) );
    ( "operator application, GT - poorly typed, first expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (typeinfer (AppOp (GT, Boolean false, Number 2)))) );
    ( "operator application, GT - poorly typed, second expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (typeinfer (AppOp (GT, Number 1, Boolean false)))) );
    ( "operator application, GTE - well typed" >:: fun _ ->
      assert_equal (Right Bool) (typeinfer (AppOp (GTE, Number 1, Number 2))) );
    ( "operator application, GTE - poorly typed, first expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (typeinfer (AppOp (GTE, Boolean false, Number 2)))) );
    ( "operator application, GTE - poorly typed, second expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (typeinfer (AppOp (GTE, Number 1, Boolean false)))) );
    ( "operator application, EQ - well typed" >:: fun _ ->
      assert_equal (Right Bool) (typeinfer (AppOp (EQ, Number 1, Number 2))) );
    ( "operator application, EQ - poorly typed, first expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (typeinfer (AppOp (EQ, Boolean false, Number 2)))) );
    ( "operator application, EQ - poorly typed, second expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (typeinfer (AppOp (EQ, Number 1, Boolean false)))) );
    ( "operator application, And - well typed" >:: fun _ ->
      assert_equal (Right Bool)
        (typeinfer (AppOp (And, Boolean true, Boolean true))) );
    ( "operator application, And - poorly typed, first expression isn't boolean"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (typeinfer (AppOp (And, Number 1, Boolean false)))) );
    ( "operator application, And - poorly typed, second expression isn't number"
    >:: fun _ ->
      assert_bool "failed on applcation operator"
        (is_left (typeinfer (AppOp (And, Boolean false, Number 1)))) );
  ]

let letTests =
  [
    ( "let - well typed" >:: fun _ ->
      assert_equal (Right Int)
        (typeinfer (Let ("x", Int, Number 1, AppOp (Add, Var "x", Number 2))))
    );
    ( "let - poorly typed, var isn't the type it is specified" >:: fun _ ->
      assert_bool "failed on let"
        (is_left
           (typeinfer
              (Let ("x", Int, Boolean true, AppOp (Add, Var "x", Number 2)))))
    );
    ( "let - poorly typed, body poorly typed" >:: fun _ ->
      assert_bool "failed on let"
        (is_left
           (typeinfer
              (Let ("x", Int, Number 1, AppOp (Add, Var "y", Number 2))))) );
  ]

let letRecTests =
  [
    ( "let rec - well typed" >:: fun _ ->
      assert_equal (Right Int)
        (typeinfer
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
    ( "let rec - poorly typed, e1 isn't of type T'" >:: fun _ ->
      assert_bool "failed on let rec"
        (is_left
           (typeinfer
              (LetRec
                 ( "fat",
                   Int,
                   Int,
                   "x",
                   If
                     (AppOp (EQ, Var "x", Number 0), Boolean true, Boolean false),
                   App (Var "fat", Number 5) )))) );
    ( "let rec - poorly typed, e2 is poorly typed" >:: fun _ ->
      assert_bool "failed on let rec"
        (is_left
           (typeinfer
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
                           App (Var "fat", AppOp (Sub, Var "x", Boolean false))
                         ) ),
                   App (Var "fat", Number 5) )))) );
  ]

let fstTests =
  [
    ( "fst - well typed" >:: fun _ ->
      assert_equal (Right Int)
        (typeinfer (Fst (Tuple (Number 1, Boolean false)))) );
    ( "fst - poorly typed" >:: fun _ ->
      assert_bool "failed on fst"
        (is_left (typeinfer (Fst (Tuple (Var "x", Boolean false))))) );
  ]

let sndTests =
  [
    ( "snd - well typed" >:: fun _ ->
      assert_equal (Right Bool)
        (typeinfer (Snd (Tuple (Number 1, Boolean false)))) );
    ( "snd - poorly typed" >:: fun _ ->
      assert_bool "failed on snd"
        (is_left (typeinfer (Snd (Tuple (Var "x", Boolean false))))) );
  ]

let nilTests =
  [
    ( "nil - well typed" >:: fun _ ->
      assert_equal (Right (List Int)) (typeinfer (Nil Int)) );
  ]

let consTests =
  [
    ( "cons - well typed" >:: fun _ ->
      assert_equal (Right (List Int)) (typeinfer (Cons (Number 1, Nil Int))) );
    ( "cons - poorly typed" >:: fun _ ->
      assert_bool "failed on cons"
        (is_left (typeinfer (Cons (Var "x", Nil Int)))) );
    ( "cons - non homogeneous list" >:: fun _ ->
      assert_bool "failed on cons"
        (is_left (typeinfer (Cons (Number 1, Cons (Boolean false, Nil Int)))))
    );
  ]

let headTests =
  [
    ( "head - well typed" >:: fun _ ->
      assert_equal (Right Int) (typeinfer (Head (Cons (Number 1, Nil Int)))) );
    ( "head - nil" >:: fun _ ->
      assert_equal (Right Int) (typeinfer (Head (Nil Int))) );
    ( "head - poorly typed, argument isn't a list" >:: fun _ ->
      assert_bool "failed on head" (is_left (typeinfer (Head (Number 1)))) );
    ( "head - poorly typed, argument is poorly typed" >:: fun _ ->
      assert_bool "failed on head"
        (is_left (typeinfer (Head (Cons (Var "x", Nil Int))))) );
  ]

let tailTests =
  [
    ( "tail - well typed" >:: fun _ ->
      assert_equal (Right (List Int))
        (typeinfer (Tail (Cons (Number 1, Nil Int)))) );
    ( "tail - nil" >:: fun _ ->
      assert_equal (Right (List Int)) (typeinfer (Tail (Nil Int))) );
    ( "tail - poorly typed, argument isn't a list" >:: fun _ ->
      assert_bool "failed on tail" (is_left (typeinfer (Tail (Number 1)))) );
    ( "tail - poorly typed, argument is poorly typed" >:: fun _ ->
      assert_bool "failed on tail"
        (is_left (typeinfer (Tail (Cons (Var "x", Nil Int))))) );
  ]

let nothingTests =
  [
    ( "nothing - well typed" >:: fun _ ->
      assert_equal (Right (Maybe Int)) (typeinfer (Nothing Int)) );
  ]

let justTests =
  [
    ( "just - well typed" >:: fun _ ->
      assert_equal (Right (Maybe Int)) (typeinfer (Just (Number 1))) );
    ( "just - poorly typed" >:: fun _ ->
      assert_bool "failed on just" (is_left (typeinfer (Just (Var "x")))) );
  ]

let matchListTests =
  [
    ( "match list - well typed, uses x" >:: fun _ ->
      assert_equal (Right Int)
        (typeinfer
           (MatchList
              (Nil Int, Number 0, "x", "xs", AppOp (Add, Number 1, Var "x"))))
    );
    ( "match list - well typed, uses xs" >:: fun _ ->
      assert_equal (Right Int)
        (typeinfer
           (MatchList
              ( Nil Int,
                Number 0,
                "x",
                "xs",
                AppOp (Add, Number 1, Head (Var "xs")) ))) );
    ( "match list - well typed, uses neither x or xs" >:: fun _ ->
      assert_equal (Right Int)
        (typeinfer
           (MatchList
              (Nil Int, Number 0, "x", "xs", AppOp (Add, Number 1, Number 2))))
    );
    ( "match list - well typed, uses both x and xs" >:: fun _ ->
      assert_equal (Right Int)
        (typeinfer
           (MatchList
              ( Nil Int,
                Number 0,
                "x",
                "xs",
                AppOp (Add, Number 1, AppOp (Mul, Var "x", Head (Var "xs"))) )))
    );
    ( "match list - poorly typed, e1 isn't a list" >:: fun _ ->
      assert_bool "failed on match list"
        (is_left
           (typeinfer
              (MatchList
                 (Number 1, Number 0, "x", "xs", AppOp (Add, Number 1, Var "x")))))
    );
    ( "match list - poorly typed, e2 and e3 have different types" >:: fun _ ->
      assert_bool "failed on match list"
        (is_left
           (typeinfer
              (MatchList
                 ( Nil Int,
                   Boolean true,
                   "x",
                   "xs",
                   AppOp (Add, Number 1, Var "x") )))) );
  ]

let matchMaybeTests =
  [
    ( "match maybe - well typed, uses x" >:: fun _ ->
      assert_equal (Right Int)
        (typeinfer
           (MatchMaybe
              (Nothing Int, Number 0, "x", AppOp (Add, Number 1, Var "x")))) );
    ( "match maybe - well typed, does not uses x" >:: fun _ ->
      assert_equal (Right Int)
        (typeinfer
           (MatchMaybe
              (Nothing Int, Number 0, "x", AppOp (Add, Number 1, Number 2)))) );
    ( "match maybe - poorly typed, e1 isn't a maybe" >:: fun _ ->
      assert_bool "failed on match maybe"
        (is_left
           (typeinfer
              (MatchMaybe
                 (Number 1, Number 0, "x", AppOp (Add, Number 1, Var "x"))))) );
    ( "match maybe - poorly typed, e2 and e3 have different types" >:: fun _ ->
      assert_bool "failed on match maybe"
        (is_left
           (typeinfer
              (MatchMaybe
                 (Nothing Int, Boolean true, "x", AppOp (Add, Number 1, Var "x")))))
    );
  ]

let isEmptyTests =
  [
    ( "is empty - well typed, nil" >:: fun _ ->
      assert_equal (Right Bool) (typeinfer (IsEmpty (Nil Int))) );
    ( "is empty - well typed, cons" >:: fun _ ->
      assert_equal (Right Bool) (typeinfer (IsEmpty (Cons (Number 3, Nil Int))))
    );
    ( "is empty - poorly typed, argument isn't a list" >:: fun _ ->
      assert_bool "failed on is empty"
        (is_left (typeinfer (IsEmpty (Boolean true)))) );
    ( "is empty - poorly typed, argument is poorly typed" >:: fun _ ->
      assert_bool "failed on is empty" (is_left (typeinfer (IsEmpty (Var "x"))))
    );
  ]

let isNothingTests =
  [
    ( "is nothing - well typed, nothing" >:: fun _ ->
      assert_equal (Right Bool) (typeinfer (IsNothing (Nothing Int))) );
    ( "is nothing - well typed, just" >:: fun _ ->
      assert_equal (Right Bool) (typeinfer (IsNothing (Just (Number 3)))) );
    ( "is nothing - poorly typed, argument isn't a list" >:: fun _ ->
      assert_bool "failed on is nothing"
        (is_left (typeinfer (IsNothing (Boolean true)))) );
    ( "is nothing - poorly typed, argument is poorly typed" >:: fun _ ->
      assert_bool "failed on is nothing"
        (is_left (typeinfer (IsNothing (Var "x")))) );
  ]

let fromJusttests =
  [
    ( "from just - well typed, nothing" >:: fun _ ->
      assert_equal (Right Int) (typeinfer (FromJust (Nothing Int))) );
    ( "from just - well typed, just" >:: fun _ ->
      assert_equal (Right Int) (typeinfer (FromJust (Just (Number 3)))) );
    ( "from just - poorly typed, argument isn't a maybe" >:: fun _ ->
      assert_bool "failed on from just"
        (is_left (typeinfer (FromJust (Boolean true)))) );
    ( "from just - poorly typed, argument is poorly typed" >:: fun _ ->
      assert_bool "failed on from just"
        (is_left (typeinfer (FromJust (Var "x")))) );
  ]

let tests =
  "test suite for typeinfer"
  >::: simpleTests @ ifTests @ tupleTests @ varTests @ functionTests @ appTests
       @ appOpTests @ letTests @ letRecTests @ fstTests @ sndTests @ nilTests
       @ consTests @ headTests @ tailTests @ nothingTests @ justTests
       @ matchListTests @ matchMaybeTests @ isEmptyTests @ isNothingTests
       @ fromJusttests

let _ = run_test_tt_main tests
