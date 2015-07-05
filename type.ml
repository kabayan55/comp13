(* �桼���ץ����η���ɽ���� *)
type t = Int
       | Float
       | Fun of t list * t    (* t list �������η��Υꥹ�ȡ�t ���֤��ͤη� *)
       | TVar of t option ref (* ���ѿ� *)

(* ���������ѿ����� *)
(* Type.gen_type : unit -> Type.t *)
let gen_type () = TVar (ref None)

(* Type.print: ����ץ��Ȥ���ؿ��ʥǥХå��ѡ�  *)
let rec to_string t = match t with
    Int -> "int"
  | Float -> "float"
  | Fun ([], t) -> failwith "argument type empty"
  | Fun (t :: ts, t') ->
	"("
	^ List.fold_left (fun str arg -> str ^ " -> " ^ to_string arg)
			 (to_string t)
			 ts
	^ " -> "
	^ to_string t'
	^ ")"
  | TVar (r) -> failwith "type variable found"

let print t =
  let str = to_string t
  in (print_string str;
      print_newline ())
