(* １階の言語に対するレジスタ割当の前処理プログラムのメイン *)

exception NorSupported

let rec g expr = match expr with
    First.Number (num) -> First.Number (num)
  | First.Real (f) -> First.Real (f)
  | First.Variable (name) -> First.Variable (name)
  | First.Op (arg1, op, arg2) -> First.Op (arg1, op, arg2)
  | First.IfEqual (arg1, arg2, arg3, arg4) ->
	First.IfEqual (arg1, arg2, g arg3, g arg4)
  | First.IfLess (arg1, arg2, arg3, arg4) ->
	First.IfLess (arg1, arg2, g arg3, g arg4)
  | First.Let ((name, typ), arg1, arg2) ->
	First.Let ((name, typ), g arg1, g arg2)

  | First.Application (name, args) ->
      let rec f args lst int = match args with
		[] -> First.Application(name, lst)
	      | first :: rest ->
		  let r = (Register.make_register int) in
		    First.Let((r, Type.gen_type()), First.Variable(first), f rest (lst @ [r])(int+1)) in (f args [] 1)  

let rec g_def def = match def with
    First.FunDef((name,typ), args, arg1) ->
      let rec f args lst int = match args with
	  [] -> lst
	| first :: rest ->
	    let r = (Register.make_register int) in
	      f rest (lst @ [(r, Type.gen_type())])(int+1) in
      let rlst = f args [] 1 in
	First.FunDef((name, typ), rlst,
		     let rec f2 args rlst = match rlst with
			 [] -> g arg1
		       | (first, typ) :: rest -> match args with
			     [] -> g arg1
			   | first2 :: rest2 ->
			       First.Let(first2, First.Variable(first), f2 rest2 rest) in (f2 args rlst))


let rec g_program program = match program with
    First.Program (args, arg1) ->
      let rec f args = match args with
	  [] -> []
	| first :: rest -> (g_def first) :: (f rest) in
	First.Program(f args, g arg1)


(* Prealloc.f: １階の言語に対するレジスタ割り当て前処理プログラムの入口 *)
(* Prealloc.f : First.prog_t -> Prealloc.prog_t *)

let f expr = g_program expr



