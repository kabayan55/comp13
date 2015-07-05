(* i �������� _R_i ���� *)
(* make_register : int -> string *)
let make_register i = "_R_" ^ (string_of_int i)

(* j �������� _F_j ���� *)
(* make_fregister : int -> string *)
let make_fregister j = "_F_" ^ (string_of_int j)

(* �ѿ�̾���쥸�������ɤ�����Ƚ�ꤹ�� *)
(* ��ư�������쥸�����Ǥ� true ���֤� *)
(* is_register : string -> bool *)
let is_register r = String.get r 0 = '_'

(* �ѿ�̾����ư�������쥸�������ɤ�����Ƚ�ꤹ�� *)
(* is_fregister : string -> bool *)
let is_fregister f = String.get f 0 = '_' && String.get f 1 = 'F'
