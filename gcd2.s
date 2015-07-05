#define _R_ax %rax
#define _R_0  %rbx
#define _R_1  %rcx
#define _R_dx %rdx
#define _R_2  %rsi
#define _R_3  %rdi
#define _R_bp %rbp
#define _R_sp %rsp
#define _R_4  %r8
#define _R_5  %r9
#define _R_6  %r10
#define _R_7  %r11
#define _R_8  %r12
#define _R_9  %r13
#define _R_10 %r14
#define _R_11 %r15

let rec gcd2_27 m_28 n_29 =
  let v2_31=0 in
  if v1_30=v2_31
  then m_28
  else let amari_32=let v1_33=m_28 in
                    let v2_34=n_29 in
                    (v1_33 mod v2_34) in
       let v_35=n_29 in
       let v_36=amari_32 in
       (gcd2_27 v_35 v_36)
in

let rec gcd_37 m_38 n_39 =
  if n_39<m_38
  then (gcd2_27 m_38 n_39)
  else (gcd2_27 n_39 m_38)
in

let rec loop1_46 n_47 =
  let v_49=18 in
  let result_48=let v_50=24 in
                (gcd_37 v_49 v_50) in
  let v1_51=n_47 in
  let v2_52=0 in
  if v1_51=v2_52
  then result_48
  else let v_53=let v1_54=n_47 in
                let v2_55=1 in
                (v1_54-v2_55) in
       (loop1_46 v_53)
in

let rec loop2_56 n_57 =
  let v_59=20000 in
  let result_58=(loop1_46 v_59) in
  let v1_60=n_57 in
  let v2_61=0 in
  if v1_60=v2_61
  then result_58
  else let v_62=let v1_63=n_57 in
                let v2_64=1 in
                (v1_63-v2_64) in
       (loop2_56 v_62)
in

let v_65=20000 in
(loop2_56 v_65)
