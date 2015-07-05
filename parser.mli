type token =
  | LPAREN
  | RPAREN
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | PLUSDOT
  | MINUSDOT
  | TIMESDOT
  | DIVIDEDOT
  | LET
  | REC
  | IN
  | IF
  | THEN
  | ELSE
  | EQUAL
  | UNEQUAL
  | EQUALORLESSTHAN
  | EQUALORMORETHAN
  | LESSTHAN
  | MORETHAN
  | DOT
  | INTEGER of (int)
  | FLOAT of (float)
  | VARIABLE of (string)
  | EOF

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.t
