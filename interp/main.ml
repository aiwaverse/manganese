open Manganese.Expr

exception ParseError

let parse (s : string) : expr =
  try
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.read lexbuf in
    ast
  with _ -> raise ParseError
