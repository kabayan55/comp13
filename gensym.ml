(* Gensym.f : string -> string *)

(* ���� var �θ��� _n��n �� counter ���͡ˤ��դ���ʸ������֤� *)
(* �Ť� _n �������դ��Ƥ����顢����򳰤����դ�ľ�� *)

let counter = ref 0

let f var =
  if var = "_" then var
  else (counter := !counter + 1;
	if String.contains var '_'
	then String.sub var 0 (String.index var '_')
		 ^ "_" ^ string_of_int !counter
	else var ^ "_" ^ string_of_int !counter)
