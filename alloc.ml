(* �쥸����������ƥץ����Υᥤ�� *)
let type_on = ref false (* ����������������� true �ˤ��� *)
exception NotSupported

(* counter:�쥸�������ֹ�Υ����󥿡� *)
let counter = ref 11

(* Alloc.g: First.t * (�ѿ� -> �ѿ�)-> First.t *)
let rec g expr env = match expr with
    First.Number (num) -> First.Number (num)
  | First.Real (f) -> First.Real (f)
  | First.Variable (name) ->
      if(Register.is_register name)
      then First.Variable (name)
      else First.Variable(Env.get env name)
  | First.Op (arg1, op, arg2) -> First.Op (Env.get env arg1, op, Env.get env arg2)
  | First.IfEqual (arg1, arg2, arg3, arg4) ->
	First.IfEqual (Env.get env arg1, Env.get env arg2, g arg3 env, g arg4 env)
  | First.IfLess (arg1, arg2, arg3, arg4) ->
	First.IfLess (Env.get env arg1, Env.get env arg2, g arg3 env, g arg4 env)
  | First.Let ((r, typ), arg1, arg2) ->
      if(Register.is_register r)
      then First.Let ((r, Type.gen_type()), g arg1 env, g arg2 env)
      else let ri = (Register.make_register !counter) in
	counter := !counter - 1;
	First.Let((ri, Type.gen_type()), g arg1 env, g arg2(Env.add env r ri))
  | First.Application (name, args) ->
     First.Application (name, args)

(* Alloc.g_def: First.def_t * (�ѿ� -> �ѿ�) -> First.def_t *)

let rec g_def def env = match def with
    First.FunDef((name,typ), args, arg1) ->
      counter := 11;
     First.FunDef((name, typ), args, g arg1 env)

(* Alloc.g_program: First.prog_t * (�ѿ� -> �ѿ� )-> First.prog_t *)
let rec g_program program env = match program with
    First.Program (args, arg1) ->
      let rec f args env = match args with
	  [] -> []
	| first :: rest -> (g_def first env) :: (f rest env) in
	First.Program(f args env, g arg1 env)

(* First.f: �����θ�����Ф���쥸����������Ƥ��������ץ��������� *)
(* Alloc.f : First.prog_t -> First.prog_t *)
let f expr = g_program expr []
