# 1 "lexer.mll"
 
(* 補助的な変数、関数、型などの定義 *)
open Parser

# 7 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\225\255\226\255\075\000\017\000\230\255\003\000\014\000\
    \236\255\160\000\235\000\054\001\129\001\204\001\023\002\012\000\
    \013\000\031\000\032\000\252\255\002\000\002\000\070\000\254\255\
    \246\255\245\255\244\255\243\255\098\002\173\002\248\002\067\003\
    \142\003\217\003\036\004\111\004\186\004\005\005\080\005\155\005\
    \230\005\049\006\124\006\234\255\235\255\233\255\033\000";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\028\000\026\000\255\255\024\000\023\000\
    \255\255\028\000\028\000\028\000\028\000\028\000\028\000\007\000\
    \006\000\005\000\004\000\255\255\002\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\028\000\028\000\008\000\028\000\
    \013\000\028\000\014\000\016\000\015\000\028\000\028\000\017\000\
    \028\000\028\000\018\000\255\255\255\255\255\255\027\000";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\255\255\255\255\000\000\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\022\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\000\000\000\000\255\255";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\021\000\021\000\021\000\021\000\021\000\000\000\021\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \021\000\000\000\021\000\000\000\000\000\000\000\000\000\000\000\
    \020\000\019\000\016\000\018\000\022\000\017\000\005\000\015\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\027\000\026\000\007\000\008\000\006\000\046\000\
    \045\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\043\000\044\000\025\000\024\000\000\000\
    \023\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\003\000\003\000\003\000\009\000\003\000\003\000\
    \003\000\011\000\003\000\003\000\013\000\014\000\003\000\003\000\
    \003\000\003\000\012\000\003\000\010\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\040\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\037\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\035\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\036\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\003\000\003\000\003\000\033\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\003\000\003\000\
    \003\000\031\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\029\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\003\000\003\000\
    \003\000\030\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\032\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\003\000\034\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\003\000\003\000\
    \003\000\003\000\038\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\039\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\041\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\003\000\003\000\003\000\042\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\021\000\021\000\000\000\255\255\021\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\021\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\020\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\015\000\016\000\000\000\000\000\000\000\004\000\
    \006\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\007\000\007\000\017\000\018\000\255\255\
    \022\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\022\000\255\255\
    \255\255\255\255\255\255\255\255\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\255\255\255\255\255\255\255\255\255\255\255\255\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\255\255\255\255\255\255\255\255\255\255\255\255\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\255\255\255\255\255\255\255\255\255\255\255\255\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token lexbuf =
    __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 15 "lexer.mll"
         ( token lexbuf )
# 533 "lexer.ml"

  | 1 ->
# 17 "lexer.mll"
  ( token lexbuf )
# 538 "lexer.ml"

  | 2 ->
# 18 "lexer.mll"
       ( LPAREN )
# 543 "lexer.ml"

  | 3 ->
# 19 "lexer.mll"
       ( RPAREN )
# 548 "lexer.ml"

  | 4 ->
# 20 "lexer.mll"
       ( PLUS )
# 553 "lexer.ml"

  | 5 ->
# 21 "lexer.mll"
       ( MINUS )
# 558 "lexer.ml"

  | 6 ->
# 22 "lexer.mll"
       ( TIMES )
# 563 "lexer.ml"

  | 7 ->
# 23 "lexer.mll"
       ( DIVIDE )
# 568 "lexer.ml"

  | 8 ->
# 24 "lexer.mll"
         ( MOD )
# 573 "lexer.ml"

  | 9 ->
# 26 "lexer.mll"
         ( PLUSDOT )
# 578 "lexer.ml"

  | 10 ->
# 27 "lexer.mll"
         ( MINUSDOT )
# 583 "lexer.ml"

  | 11 ->
# 28 "lexer.mll"
         ( TIMESDOT )
# 588 "lexer.ml"

  | 12 ->
# 29 "lexer.mll"
         ( DIVIDEDOT )
# 593 "lexer.ml"

  | 13 ->
# 30 "lexer.mll"
         ( LET )
# 598 "lexer.ml"

  | 14 ->
# 31 "lexer.mll"
         ( REC )
# 603 "lexer.ml"

  | 15 ->
# 32 "lexer.mll"
         ( IN )
# 608 "lexer.ml"

  | 16 ->
# 33 "lexer.mll"
         ( IF )
# 613 "lexer.ml"

  | 17 ->
# 34 "lexer.mll"
         ( THEN )
# 618 "lexer.ml"

  | 18 ->
# 35 "lexer.mll"
         ( ELSE )
# 623 "lexer.ml"

  | 19 ->
# 36 "lexer.mll"
         ( EQUAL )
# 628 "lexer.ml"

  | 20 ->
# 37 "lexer.mll"
         ( UNEQUAL )
# 633 "lexer.ml"

  | 21 ->
# 38 "lexer.mll"
         ( EQUALORLESSTHAN )
# 638 "lexer.ml"

  | 22 ->
# 39 "lexer.mll"
         ( EQUALORMORETHAN )
# 643 "lexer.ml"

  | 23 ->
# 40 "lexer.mll"
         ( LESSTHAN )
# 648 "lexer.ml"

  | 24 ->
# 41 "lexer.mll"
         ( MORETHAN )
# 653 "lexer.ml"

  | 25 ->
# 43 "lexer.mll"
         ( DOT )
# 658 "lexer.ml"

  | 26 ->
# 46 "lexer.mll"
  ( INTEGER (int_of_string (Lexing.lexeme lexbuf)) )
# 663 "lexer.ml"

  | 27 ->
# 49 "lexer.mll"
  ( FLOAT(float_of_string (Lexing.lexeme lexbuf)) )
# 668 "lexer.ml"

  | 28 ->
# 52 "lexer.mll"
  ( VARIABLE (Lexing.lexeme lexbuf))
# 673 "lexer.ml"

  | 29 ->
# 55 "lexer.mll"
       ( EOF )
# 678 "lexer.ml"

  | 30 ->
# 56 "lexer.mll"
     ( failwith ("unknown token: " ^ Lexing.lexeme lexbuf) )
# 683 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;

