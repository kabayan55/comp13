{
(* ���Ū���ѿ����ؿ������ʤɤ���� *)
open Parser
}

(* ����ɽ����ά�� *)
(* [...] ����� character '...' �Ǥʤ��ƤϤʤ�ʤ� *)
let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper

rule token = parse
| space+ { token lexbuf }       (* ���ڡ������ɤ����Ф� *)
| "(*" [^ '\n']* "\n"           (* ( * ��������ޤǤϥ����� *)
	 { token lexbuf }
| "("	 { LPAREN }
| ")"	 { RPAREN }
| "+"	 { PLUS }
| "-"	 { MINUS }
| "*"	 { TIMES }
| "/"	 { DIVIDE }
| "mod"	 { MOD }
(* ���Σ��Ĥ�float�ΤȤ��η׻��ΤȤ��ˤĤ����� *)
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
(* ����������ʤ���float�Ǥ��ʤ���͡� *)
| "."    { DOT }

| digit+                        (* ���������İʾ� *)
	 { INTEGER (int_of_string (Lexing.lexeme lexbuf)) }
	 (* �¿�����������ޤ�������� *)
| digit+ '.' digit*
	 { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
	(* �ѿ�������ե��٥åȤǻϤޤꡢ����ե��٥åȤޤ��Ͽ�����³������� *) 
| lower+ ( alpha | digit)*
	 { VARIABLE (Lexing.lexeme lexbuf)}


| eof	 { EOF }                (* ���Ͻ�λ *)
| _	 { failwith ("unknown token: " ^ Lexing.lexeme lexbuf) }


(* 3 �Ȥ���ȡ������lexer.mll�Ǥ�����Ф��������� *)
(* �������äƤ뤫�ϼ����ʤ����ɰ��������������Ϥ����� *)
