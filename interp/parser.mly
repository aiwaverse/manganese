%{
open Manganese.Types
open Manganese.Expr
%}
%token <int> INT
%token <string> ID
%token TRUE
%token FALSE
%token LEQ
%token GEQ
%token LESSER
%token GREATER
%token TIMES
%token PLUS
%token MINUS
%token DIV
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token EOF
%token COLON
%token ARROW
%token TINT
%token TBOOL
%token FN
%token REC
%token OPENBRACKETS
%token CLOSEBRACKETS
%token MAYBE
%token COMMA
%token FUNCTIONARROW
%token FST
%token SND
%token CONS
%token NIL
%token HEAD
%token TAIL
%token JUST
%token NOTHING
%token MATCH
%token WITH
%token PIPE
%token AND
%token OR

%nonassoc IN
%nonassoc ELSE
%left AND
%left OR
%left LEQ
%left GEQ
%left LESSER
%left GREATER
%left PLUS
%left MINUS
%left TIMES
%left DIV
%left HEAD
%left TAIL
%left FST
%left SND
%left Application
%right CONS

%start <Manganese.Expr.expr> prog
%%

prog:
  | e = expr; EOF { e }
  ;

types:
  | TINT { Int }
  | TBOOL { Bool }
  | t1 = types; ARROW; t2 = types { Func (t1, t2) }
  | OPENBRACKETS; t = types; CLOSEBRACKETS { List t }
  | MAYBE; t = types { Maybe t }
  | LPAREN; t1 = types; COMMA; t2 = types; RPAREN { Pair (t1, t2) }

expr:
  | i = INT { Number i }
  | TRUE { Boolean true }
  | FALSE { Boolean false }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | FN; x = ID; COLON; t = types; FUNCTIONARROW; body = expr { Function(x, t, body) }
  | e1 = expr; e2 = expr { App (e1, e2) } %prec Application
  | e1 = expr; TIMES; e2 = expr { AppOp (Mul, e1, e2) }
  | e1 = expr; PLUS; e2 = expr { AppOp (Add, e1, e2) }
  | e1 = expr; MINUS; e2 = expr { AppOp (Sub, e1, e2) }
  | e1 = expr; DIV; e2 = expr { AppOp (Div, e1, e2) }
  | e1 = expr; LEQ; e2 = expr { AppOp (LTE, e1, e2) }
  | e1 = expr; GEQ; e2 = expr { AppOp (GTE, e1, e2) }
  | e1 = expr; EQUALS; e2 = expr { AppOp (EQ, e1, e2) }
  | e1 = expr; LESSER; e2 = expr { AppOp (LT, e1, e2) }
  | e1 = expr; GREATER; e2 = expr { AppOp (GT, e1, e2) }
  | e1 = expr; AND; e2 = expr { AppOp (And, e1, e2) }
  | e1 = expr; OR; e2 = expr { AppOp (Or, e1, e2) }
  | LET; x = ID; COLON; t = types; EQUALS; e1 = expr; IN; e2 = expr { Let (x, t, e1, e2) }
  | LET; REC; f = ID; COLON; t1 = types; ARROW; t2 = types; EQUALS; FN; x = ID; COLON; types; FUNCTIONARROW; e1 = expr; IN; e2 = expr { LetRec (f, t1, t2, x, e1, e2) } 
  | LET; REC; f = ID; LPAREN; x = ID; COLON; t1 = types; RPAREN; COLON; t2 = types; EQUALS; e1 = expr; IN; e2 = expr { LetRec (f, t1, t2, x, e1, e2) }
  | x = ID { Var x }
  | LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN { Tuple(e1, e2) }
  | FST; e1 = expr { Fst e1 }
  | SND; e1 = expr { Snd e1 }
  | NIL; COLON; t = types { Nil t }
  | e1 = expr; CONS; e2 = expr { Cons (e1, e2) }
  | HEAD; e1 = expr { Head e1 }
  | TAIL; e1 = expr { Tail e1 }
  | MATCH; e1 = expr; WITH; NIL; FUNCTIONARROW; e2 = expr; PIPE; x = ID; CONS; xs = ID; FUNCTIONARROW; e3 = expr { MatchList (e1, e2, x, xs, e3) }
  | JUST; e = expr { Just e }
  | NOTHING; COLON; t = types { Nothing t }
  | MATCH; e1 = expr; WITH; NOTHING; FUNCTIONARROW; e2 = expr; PIPE; JUST; x = ID; FUNCTIONARROW; e3 = expr { MatchMaybe (e1, e2, x, e3) }
  | LPAREN; e=expr; RPAREN { e }
  | OPENBRACKETS; vl = list_fields; CLOSEBRACKETS; COLON; OPENBRACKETS; t = types; CLOSEBRACKETS { List.fold_right (fun elem acc -> Cons(elem, acc)) vl (Nil t)}
  | FN; x = list(argument_list); FUNCTIONARROW; body = expr { List.fold_right (fun (e, t) acc -> Function(e, t, acc)) x body }
  ; 

list_fields:
  vl = separated_list(COMMA, expr) { vl };

argument_list:
  LPAREN; e = ID; COLON; t = types; RPAREN; { (e, t) }