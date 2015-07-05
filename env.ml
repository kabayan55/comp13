(* �Ķ���
   �ѿ� key ���ͤ� value �Ǥ��뤳�Ȥ��ݻ�����ơ��֥� *)

(* key �����Ĥ���ʤ��ä��Ȥ��� raise ������㳰 *)
exception UnboundVariable of string

(* ���δĶ� *)
let empty_env = []

(* �Ķ� env �� (key, value) ��ä��� *)
let add env key value = (key, value) :: env

(* �Ķ� env ���� key ���б������ͤ���Ф� *)
let rec get env key = match env with
    [] -> raise (UnboundVariable key)
  | (first_key, first_value) :: rest ->
	if key = first_key then first_value
			   else get rest key
