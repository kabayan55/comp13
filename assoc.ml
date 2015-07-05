(* letの結合性変換 *)

open Knormal

(* メイン *)

let rec g expr = match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> expr
  | Op (name1, op, name2) -> expr
  | IfEqual (name1, name2, expr3, expr4) ->
	IfEqual (name1, name2, g expr3, g expr4)
  | IfLess (name1, name2, expr3, expr4) ->
	IfLess (name1, name2, g expr3, g expr4)
  | Let ((p, t1), Let((q,t2), exprb, exprc), expra) ->
      Let ((q, Type.gen_type()), g exprb, Let((p, Type.gen_type()), g exprc, expra))
  | Let ((name, t), expr1, expr2) ->
	Let ((name, t), expr1, expr2)
  | LetRec ((name, t), args, expr1, expr2) ->
	LetRec ((name, t), args, g expr1, g expr2)
  | Application (name, name_list) -> expr

(* eta.f : Knormal.t -> Knormal.t *)

let f expr = g expr 
