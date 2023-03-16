open Interp.Main
open Manganese.Expr
open Manganese.Typeinfer
open Manganese.Types
open Manganese.Eval
open Either

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
  try
    while true do
      print_string "\n> ";
      let inputMessage = read_line () in
      let parsedInput = parse inputMessage in
      if !verbose_parser then
        print_endline ("Parsed expression: " ^ print_expr parsedInput);
      let inferredType = typeinfer parsedInput in
      match inferredType with
      | Left err -> print_endline err
      | Right t -> (
          if !verbose_typeinfer then
            print_endline ("Expression type: " ^ print_l1Type t);
          let evaluatedExpr = eval parsedInput in
          match evaluatedExpr with
          | Left err -> print_endline ("Evaluation failed. " ^ err)
          | Right v -> print_endline (print_expr v))
    done
  with End_of_file -> ()
