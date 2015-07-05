{
(* 補助的な変数、関数、型などの定義 *)
open Parser
}

(* 正規表現の略記 *)
(* [...] の中は character '...' でなくてはならない *)
let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper

rule token = parse
| space+ { token lexbuf }       (* スペースは読み飛ばす *)
| "(*" [^ '\n']* "\n"           (* ( * から行末まではコメント *)
	 { token lexbuf }
| "("	 { LPAREN }
| ")"	 { RPAREN }
| "+"	 { PLUS }
| "-"	 { MINUS }
| "*"	 { TIMES }
| "/"	 { DIVIDE }
| "mod"	 { MOD }
(* 下の４つはfloatのときの計算のときにつかう！ *)
| "+."   { PLUSDOT }
| "-."   { MINUSDOT }
| "*."   { TIMESDOT }
| "/."   { DIVIDEDOT }
| "let"  { LET }
| "rec"  { REC }
| "in"   { IN }
| "if"   { IF }
| "then" { THEN }
| "else" { ELSE }
| "="    { EQUAL }
| "<>"   { UNEQUAL }
| "<="   { EQUALORLESSTHAN }
| ">="   { EQUALORMORETHAN }
| "<"    { LESSTHAN }
| ">"    { MORETHAN }
(* これ定義しないとfloatできないよね？ *)
| "."    { DOT }

| digit+                        (* 数字が１個以上 *)
	 { INTEGER (int_of_string (Lexing.lexeme lexbuf)) }
	 (* 実数、小数点を含む数字の列 *)
| digit+ '.' digit*
	 { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
	(* 変数、アルファベットで始まり、アルファベットまたは数字が続いたもの *) 
| lower+ ( alpha | digit)*
	 { VARIABLE (Lexing.lexeme lexbuf)}


| eof	 { EOF }                (* 入力終了 *)
| _	 { failwith ("unknown token: " ^ Lexing.lexeme lexbuf) }


(* 3 使われるトークンはlexer.mllでいじればいいんだよね *)
(* 定義が合ってるかは自信ないけど一応全部定義したはずだよ *)
