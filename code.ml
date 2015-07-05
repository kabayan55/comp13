(* Intel 用コード生成 *)

open First
open Register


(* registers *)

let r_sp = "_R_sp"
let r_bp = "_R_bp"
let r_ax = "_R_ax"
let r_dx = "_R_dx"

(* instructions *)

let label l =	  l ^ ":\n"
let movqi i r2  = "	movq $" ^ string_of_int i ^ ", " ^ r2 ^ "\n"
let movq r1 r2    = "	movq " ^ r1 ^ ", " ^ r2 ^ "\n"
let addq r1 r2  = "	addq " ^ r1 ^ ", " ^ r2 ^ "\n"
let subq r1 r2  = "	subq " ^ r1 ^ ", " ^ r2 ^ "\n"
let subqi i r2  = "	subq " ^ string_of_int i ^ ", " ^ r2 ^ "\n"
let imulq r1 r2 = "	imulq " ^ r1 ^ ", " ^ r2 ^ "\n"
let sarqi i r2 = "	sarq $" ^ string_of_int i ^ ", " ^ r2 ^ "\n"
let idivq r    = "	idivq " ^ r ^ "\n"
let cmpq r1 r2  = "	cmpq " ^ r1 ^ ", " ^ r2 ^ "\n"
let jne l =	"	jne " ^ l ^ "\n"
let jle l =	"	jle " ^ l ^ "\n"
let jmp l =	"	jmp " ^ l ^ "\n"
let pushq r =	"	pushq " ^ r ^ "\n"
let popq r  =	"	popq " ^ r ^ "\n"
let call f =	"	call " ^ f ^ "\n"
let ret =	"	ret\n"

(* headers *)

let top =	"	.text\n"
let middle =	"	\n" ^
		"	.globl _asm_main\n" ^
		"_asm_main: # main entry point\n" ^
			pushq "%rbx" ^
			pushq "%r12" ^
			pushq "%r13" ^
			pushq "%r14" ^
			pushq "%r15" ^
			pushq r_bp ^
			movq r_sp r_bp ^
		"    # main program start\n"
let last =	"    # main program end\n" ^
			movq (make_register 0) r_ax ^
			movq r_bp r_sp ^
			popq r_bp ^
			popq "%r15" ^
			popq "%r14" ^
			popq "%r13" ^
			popq "%r12" ^
			popq "%rbx" ^
			ret

(* push/pop registers *)

let rec push_live live = match live with
    [] -> ""
  | var :: rest -> pushq var ^ push_live rest

let rec pop_live live = match live with
    [] -> ""
  | var :: rest -> pop_live rest ^ popq var

(* メイン *)

exception NotSupported 

(* remove: リストから要素を取り除く関数 *)
(* remove: string list * string -> string list  *)

let rec remove lst v = match lst with
    [] -> []
  | first :: rest ->
      if first = v then rest
      else first :: remove rest v

(* fv:式Eのなかの自由変数を返す関数 *)
(* fv: First.t -> 変数の集合(String list) *)

let rec fv expr = match expr with
    First.Number (num) -> []
  | First.Real (f) -> []
  | First.Variable (name) -> [name]
  | First.Op (arg1, op, arg2) -> [arg1; arg2]
  | First.IfEqual (arg1, arg2, arg3, arg4) -> 
      [arg1; arg2] @ (fv arg3) @ (fv arg4)
  | First.IfLess (arg1, arg2, arg3, arg4) -> 
      [arg1; arg2] @ (fv arg3) @ (fv arg4)
  | First.Let ((name, typ), arg1, arg2) ->
      (fv arg1) @ (remove (fv arg2) name)
  | First.Application (arg, args) -> args




(* レジスタ割り当てのすんだ1階の言語に対するコード生成 *)
(* Code.g: First.t * 変数 * 変数の集合 -> string *)

let rec g expr z live = match expr with
    First.Number(num) -> movqi num z
  | First.Real(f) -> raise NotSupported
  | First.Variable(name) -> 
      if name = z then ""
      else movq name z
  | First.Op (arg1, Operator.Plus, arg2) ->
	movq arg1 r_ax ^
	  addq arg2 r_ax ^
	  movq r_ax z
  | First.Op (arg1, Operator.Minus, arg2) ->
	sarqi 63 r_dx ^
	  movq arg1 r_ax ^
	  subq arg2 r_ax^
	  movq r_ax z
 | First.Op (arg1, Operator.Times, arg2) ->
	sarqi 63 r_dx ^
	  movq arg1 r_ax ^
	  imulq arg2 r_ax^
	  movq r_ax z
 | First.Op (arg1, Operator.Divide, arg2) ->
	sarqi 63 r_dx ^
	  movq arg1 r_ax ^
	  idivq arg2 ^
	  movq r_ax z
 | First.Op (arg1, Operator.Mod, arg2) ->
	sarqi 63 r_dx ^
	  movq arg1 r_ax ^
	  idivq arg2 ^
	  movq r_dx z
 | First.Op (arg1, Operator.PlusDot, arg2) -> 
     raise NotSupported
 | First.Op (arg1, Operator.MinusDot, arg2) -> 
     raise NotSupported
 | First.Op (arg1, Operator.TimesDot, arg2) -> 
      raise NotSupported
 | First.Op (arg1, Operator.DivideDot, arg2) -> 
     raise NotSupported  
  | First.IfEqual (arg1, arg2, arg3, arg4) ->
      let l1 = Gensym.f "l1" in
      let l2 = Gensym.f "l2" in
	cmpq arg1 arg2 ^
	  jne l1 ^
	  g arg3 z live ^
	  jmp l2 ^
	  label l1 ^
	  g arg4 z live ^
	  label l2
  | First.IfLess (arg1, arg2, arg3, arg4) ->
      let l1 = Gensym.f "l1" in
      let l2 = Gensym.f "l2" in
	cmpq arg1 arg2 ^
	  jle l1 ^
	  g arg3 z live ^
	  jmp l2 ^
	  label l1 ^
	  g arg4 z live ^
	  label l2
  | First.Let((name, typ), arg1, arg2) ->
      g arg1 name (live @ (remove (fv arg2) name)) ^
	g arg2 z live
  | First.Application(arg, args) ->
      push_live live ^
	call arg ^
	if z = make_register 0 then "" 
	else movq (make_register 0) z ^
	  pop_live live
 


let rec g_def def = match def with
    First.FunDef ((name, typ), args, arg1) ->
      label name ^
	pushq r_bp ^
	movq r_sp r_bp ^
	g arg1 (make_register 0) [] ^
	movq r_bp r_sp ^
	popq r_bp ^
	ret

let rec g_program program = match program with
    First.Program (args, arg1) ->
      let rec f args = match args with
	  [] -> ""
	| first :: rest -> g_def first ^ f rest in
	    top ^
	   f args ^
	   middle ^
	   g arg1 (make_register 0) [] ^
	   last



(* Code.f : First.prog_t -> string *)

let f program = g_program program
