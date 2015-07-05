(* β変換 *)

open Knormal

(* x が環境の中に入っていたら変数におきかえる関数 *)

let replace env x = try Env.get env x with Env.UnboundVariable (x)->x


(* メイン *)

let rec g expr env = match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> Variable (replace env name)
  | Op (name1, op, name2) -> Op (replace env name1, op, replace env name2)
  | IfEqual (name1, name2, expr3, expr4) ->
	IfEqual (replace env name1, replace env name2,
		 g expr3 env,
		 g expr4 env)
  | IfLess (name1, name2, expr3, expr4) ->
	IfLess (replace env name1, replace env name2,
		g expr3 env,
		g expr4 env)
  | Let ((name1, t), Variable(name2), expr2) ->
      g expr2(Env.add env name1(replace env name2))
  | Let ((name, t), expr1, expr2) ->
	Let ((name, t), expr1, expr2)
  | LetRec ((name, t), params, expr1, expr2) ->
	LetRec ((name, t), params, g expr1 env, g expr2 env)
  | Application (name, name_list) ->
	Application (replace env name,
		     List.map (fun var -> replace env var) name_list)

(* Alpha.f : Knormal.t -> Knormal.t *)

let f expr = g expr Env.empty_env
