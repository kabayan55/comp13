%{
(* 補助的な変数、関数、型などの定義 *)
  open Syntax

%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token LPAREN
%token RPAREN
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token MOD
%token PLUSDOT
%token MINUSDOT
%token TIMESDOT
%token DIVIDEDOT
%token LET
%token REC
%token IN
%token IF
%token THEN
%token ELSE
%token EQUAL
%token UNEQUAL
%token EQUALORLESSTHAN
%token EQUALORMORETHAN
%token LESSTHAN
%token MORETHAN
%token DOT

%token <int> INTEGER
%token <float> FLOAT
%token <string>VARIABLE

/* これは、数字には int 型の値が伴うことを示している */
%token EOF
/* End of File: 入力の終わりを示す */

/* 非終端記号の型をここで宣言する */
%type <Syntax.t> expr

/* 開始記号の定義 */
%start expr

/* 演算子の優先順位を指定する */
/* 下に行くほど強く結合する */
%right IF THEN IN ELSE LET REC
%left EQUAL UNEQUAL EQUALORLESSTHAN EQUALORMORETHAN LESSTHAN MORETHAN
%left PLUS MINUS PLUSDOT MINUSDOT
%left TIMES DIVIDE TIMESDOT DIVIDEDOT
%left MOD
%left DOT

%nonassoc UNARY
/* nonassoc は結合なし（毎回、かっこを書かなくてはならない）、
   left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

/* 単純式 */
simple_expr:
| INTEGER
   { Syntax.Number ($1) }
| FLOAT
   { Syntax.Real ($1) }	
| VARIABLE
   { Syntax.Variable ($1) }
| LPAREN expr RPAREN
	{ $2 }

/* 式 */
expr:
| simple_expr
	{ $1 }


| expr PLUS expr
	{ Syntax.Op ($1, Operator.Plus, $3) }
| expr MINUS expr
	{ Syntax.Op ($1, Operator.Minus, $3) }
| expr TIMES expr
	{ Syntax.Op ($1, Operator.Times, $3) }
| expr DIVIDE expr
	{ Syntax.Op ($1, Operator.Divide, $3) }
| expr MOD expr
	{ Syntax.Op ($1, Operator.Mod, $3) }


| expr PLUSDOT expr
	{ Syntax.Op ($1, Operator.PlusDot, $3) }
| expr MINUSDOT expr
	{ Syntax.Op ($1, Operator.MinusDot, $3) }
| expr TIMESDOT expr
	{ Syntax.Op ($1, Operator.TimesDot, $3) }
| expr DIVIDEDOT expr
	{ Syntax.Op ($1, Operator.DivideDot, $3) }


| IF expr EQUAL expr THEN expr ELSE expr
        { Syntax.IfEqual($2, $4, $6, $8) }
| IF expr UNEQUAL expr THEN expr ELSE expr
        { Syntax.IfEqual($2, $4, $8, $6) }
| IF expr EQUALORLESSTHAN expr THEN expr ELSE expr
        { Syntax.IfLess($4, $2, $8, $6) }
| IF expr EQUALORMORETHAN expr THEN expr ELSE expr
        { Syntax.IfLess($2, $4, $8, $6) }
| IF expr LESSTHAN expr THEN expr ELSE expr
        { Syntax.IfLess($2, $4, $6, $8) }
| IF expr MORETHAN expr THEN expr ELSE expr
        { Syntax.IfLess($4, $2, $6, $8) }


| LET VARIABLE EQUAL expr IN expr
    { Syntax.Let( ($2,Type.gen_type()), $4, $6) }

| LET REC VARIABLE variable_list EQUAL expr IN expr
    { Syntax.LetRec(($3,Type.gen_type()), $4, $6, $8) }

| simple_expr exper_list
    {Syntax.Application($1, $2)}
| MINUS expr %prec UNARY
      {Syntax.Op(Syntax.Number (0), Operator.Minus, $2)}
| PLUS expr %prec UNARY
      {Syntax.Op(Syntax.Number (0), Operator.Plus, $2)}

/* 変数列 */

variable_list:
| VARIABLE
    { [($1,Type.gen_type())]}
| VARIABLE variable_list
    { ($1,Type.gen_type())::$2}



/* 単純式の列 */

exper_list:
| simple_expr 
    { [$1] }
| simple_expr exper_list
    { $1::$2 }



/*
(* 変数列と、単純式の列をなんて名前にすればいいか決めれば（わかれば）解決？ *)
(* 変数列がformal_argsで、単純式の列がactual_args？ *)

(* 変数列ってどうすればいいの？arg_list は　$2 じゃだめだからかえないと *)
(* and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t } *)
(* syntax.ml を見ながら考えよう！　オートマトンのページにも説明結構かいてある *)
(* 単純式 単純式の列 *)



(* 単項式 *)
(*| MINUS expr %prec UNARY
	{ Syntax.Op1 (Syntax.UMinus, $2) }
| PLUS expr %prec UNARY
	{ Syntax.Op1 (Syntax.UPlus, $2) }
*)


(* 4 対象言語、コンパイルされる言語をここでいじればいいんだよね？ *)
(* 単純式simple_exprの方は終わってるはず *)

 (* 式 = 単純式　単純式の列 ってどういう状況？？*)

(* シンボルの列のところには、特別に「%prec トークン」という ものを最後につけることができる。 これがついていると、この文法規則の優先順位がトークンで示される 優先順位になる。 *)
*/
