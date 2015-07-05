(* ���Ѵ�����������ŤޤǤ��Ѳ����ʤ��ʤ�ޤǷ����֤� *)
(* simple_optimize : Knormal.t -> Knormal.t *)
let rec simple_optimize kprogram0 =
  let kprogram = kprogram0 in
  let kprogram = Beta.f kprogram in                      (* ���Ѵ� *)
  let kprogram = Eta.f kprogram in 		(* ���Ѵ� *)
  let kprogram = Assoc.f kprogram in 	(* ������Ѵ� *)
  (* let kprogram = Elim.f kprogram in *)		(* �����ѿ����� *)
  (* let kprogram = Constf.f kprogram in *)		(* ������� *)
  if kprogram = kprogram0 then kprogram
			  else simple_optimize kprogram

(* �Ƽ��Ŭ���� n �󡢹Ԥ� *)
(* optimize : Knormal.t -> int -> Knormal.t *)
let rec optimize kprogram0 n =
  let kprogram = simple_optimize kprogram0 in
  if n > 0 then (* let kprogram = Expand.f kprogram in *) (* �ؿ�Ÿ�� *)
		let kprogram = simple_optimize kprogram in
		optimize kprogram (n - 1)
	   else kprogram

(* �ᥤ��ؿ� *)
let go () =
  let program = Parser.expr Lexer.token (Lexing.from_channel stdin) in
					(* ���Ϥ�ʸ���Ϥ���*)
  let kprogram = Knormal.f program in	(* k-���������Ѵ�����*)
  let kprogram = Alpha.f kprogram in	(* ���Ѵ�����*)
  let kprogram = optimize kprogram 0 in	(* �Ƽ��Ŭ����ܤ���*)
  Knormal.print kprogram		(* ɽ�����롣*)
  (*
  let fprogram = First.f kprogram in	(* �����θ�����Ѵ�����*)
  let fprogram = Prealloc.f fprogram in	(* �쥸�������������������Ԥ���*)
  let fprogram = Alloc.f fprogram in	(* �쥸����������Ƥ�Ԥ���*)
  let asm_code = Code.f fprogram in	(* ������������Ԥ���*)
  print_string asm_code			(* ɽ�����롣*)
  *)

(* �������ȥ��å� *)
let _ = go ()
