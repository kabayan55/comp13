%{
(* ���Ū���ѿ����ؿ������ʤɤ���� *)
  open Syntax

%}

/* �ʹߡ��ɤ������櫓�������Ȥ� C ���ˤʤ뤳�Ȥ���� */
/* �ȡ��������� */
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

/* ����ϡ������ˤ� int �����ͤ�ȼ�����Ȥ򼨤��Ƥ��� */
%token EOF
/* End of File: ���Ϥν����򼨤� */

/* ��ü����η��򤳤���������� */
%type <Syntax.t> expr

/* ���ϵ������� */
%start expr

/* �黻�Ҥ�ͥ���̤���ꤹ�� */
/* ���˹Ԥ��ۤɶ�����礹�� */
%right IF THEN IN ELSE LET REC
%left EQUAL UNEQUAL EQUALORLESSTHAN EQUALORMORETHAN LESSTHAN MORETHAN
%left PLUS MINUS PLUSDOT MINUSDOT
%left TIMES DIVIDE TIMESDOT DIVIDEDOT
%left MOD
%left DOT

%nonassoc UNARY
/* nonassoc �Ϸ��ʤ�����󡢤��ä���񤫤ʤ��ƤϤʤ�ʤ��ˡ�
   left �Ϻ���硢right �ϱ���� */

/* �ʲ��� %% �Ͼ�ά�Բġ�����ʹߤ�ʸˡ��§��� */
%%

/* ñ�㼰 */
simple_expr:
| INTEGER
   { Syntax.Number ($1) }
| FLOAT
   { Syntax.Real ($1) }	
| VARIABLE
   { Syntax.Variable ($1) }
| LPAREN expr RPAREN
	{ $2 }

/* �� */
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

/* �ѿ��� */

variable_list:
| VARIABLE
    { [($1,Type.gen_type())]}
| VARIABLE variable_list
    { ($1,Type.gen_type())::$2}



/* ñ�㼰���� */

exper_list:
| simple_expr 
    { [$1] }
| simple_expr exper_list
    { $1::$2 }



/*
(* �ѿ���ȡ�ñ�㼰�����ʤ��̾���ˤ���Ф���������Сʤ狼��С˲�衩 *)
(* �ѿ���formal_args�ǡ�ñ�㼰����actual_args�� *)

(* �ѿ���äƤɤ�����Ф����Ρ�arg_list �ϡ�$2 �����������餫���ʤ��� *)
(* and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t } *)
(* syntax.ml �򸫤ʤ���ͤ��褦���������ȥޥȥ�Υڡ����ˤ������빽�����Ƥ��� *)
(* ñ�㼰 ñ�㼰���� *)



(* ñ�༰ *)
(*| MINUS expr %prec UNARY
	{ Syntax.Op1 (Syntax.UMinus, $2) }
| PLUS expr %prec UNARY
	{ Syntax.Op1 (Syntax.UPlus, $2) }
*)


(* 4 �оݸ��졢����ѥ��뤵������򤳤��Ǥ�����Ф��������͡� *)
(* ñ�㼰simple_expr�����Ͻ���äƤ�Ϥ� *)

 (* �� = ñ�㼰��ñ�㼰���� �äƤɤ�������������*)

(* ����ܥ����ΤȤ���ˤϡ����̤ˡ�%prec �ȡ�����פȤ��� ��Τ�Ǹ�ˤĤ��뤳�Ȥ��Ǥ��롣 ���줬�Ĥ��Ƥ���ȡ�����ʸˡ��§��ͥ���̤��ȡ�����Ǽ������ ͥ���̤ˤʤ롣 *)
*/
