{
  open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z' '_']
let id = letter+

rule read =
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "int" { TINT }
  | "bool" { TBOOL }
  | "->" { ARROW }
  | ":" { COLON }
  | "<=" { LEQ }
  | "<" { LESSER }
  | ">=" { GEQ }
  | ">" { GREATER }
  | "*" { TIMES }
  | "+" { PLUS }
  | "-" { MINUS }
  | "&&" { AND }
  | "||" { OR }
  | "div" { DIV }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fn" { FN }
  | "rec" { REC }
  | "[" { OPENBRACKETS }
  | "]" { CLOSEBRACKETS }
  | "maybe" { MAYBE }
  | "," { COMMA }
  | "=>" { FUNCTIONARROW }
  | "fst" { FST }
  | "snd" { SND }
  | "::" { CONS }
  | "nil" { NIL }
  | "hd" { HEAD }
  | "tl" { TAIL }
  | "just" { JUST }
  | "nothing" { NOTHING }
  | "match" { MATCH }
  | "with" { WITH }
  | "|" { PIPE }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }