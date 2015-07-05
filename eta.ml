(* eta変換 *)

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
  | Let ((name1, t), expr1, Variable(name2)) ->
      if name1 = name2 then g expr1
      else Let ((name1, t), g expr1, Variable(name2))
  | Let ((name, t), expr1, expr2) ->
	Let ((name, t), expr1, expr2)
  | LetRec ((name, t), args, expr1, expr2) ->
	LetRec ((name, t), args, g expr1, g expr2)
  | Application (name, name_list) -> expr

(* eta.f : Knormal.t -> Knormal.t *)

let f expr = g expr 
