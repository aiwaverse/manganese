open Interp.Main
open Manganese.Expr
open Manganese.Typeinfer
open Manganese.Types
open Manganese.Eval
open Either
open LNoise

let rec user_input prompt verbose_parser verbose_typeinfer cb =
  match linenoise prompt with
  | None -> ()
  | Some value -> (
      cb value;
      try
        let parsedInput = parse value in
        if !verbose_parser then
          print_endline ("Parsed expression: " ^ print_raw_expr parsedInput);
        let inferredType = typeinfer parsedInput in
        match inferredType with
        | Left err ->
            print_endline err;
            user_input prompt verbose_parser verbose_typeinfer cb
        | Right t -> (
            if !verbose_typeinfer then
              print_endline ("Expression type: " ^ print_l1Type t);
            let evaluatedExpr = eval parsedInput in
            match evaluatedExpr with
            | Left err ->
                print_endline ("Evaluation failed. " ^ err);
                user_input prompt verbose_parser verbose_typeinfer cb
            | Right v ->
                print_endline (print_expr v);
                user_input prompt verbose_parser verbose_typeinfer cb)
      with ParseError ->
        print_endline "Parser error, please check input.";
        user_input prompt verbose_parser verbose_typeinfer cb)

let usage_msg = "manganese [-verbose-parser] [-verbose-typeinfer]"
let verbose_parser = ref false
let verbose_typeinfer = ref false
let input_files = ref []
let anon_fun filename = input_files := filename :: !input_files

let speclist =
  [
    ( "-verbose-parser",
      Arg.Set verbose_parser,
      "Outputs the program read by the parser" );
    ( "-verbose-typeinfer",
      Arg.Set verbose_typeinfer,
      "Outputs the type inferred" );
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;
  history_set ~max_length:100 |> ignore;
  (fun from_user ->
    if from_user = "quit" then exit 0;
    history_add from_user |> ignore)
  |> user_input "> " verbose_parser verbose_typeinfer
