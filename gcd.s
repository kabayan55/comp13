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

let rec gcd2_19 m_20 n_21 =
  let v2_23=0 in
  if v1_22=v2_23
  then m_20
  else let amari_24=let v1_25=m_20 in
                    let v2_26=n_21 in
                    (v1_25 mod v2_26) in
       let v_27=n_21 in
       let v_28=amari_24 in
       (gcd2_19 v_27 v_28)
in

let rec gcd_29 m_30 n_31 =
  if n_31<m_30
  then (gcd2_19 m_30 n_31)
  else (gcd2_19 n_31 m_30)
in

let v1_39=12 in
let v_38=let v2_40=6 in
         (v1_39+v2_40) in
let v_41=let v1_42=3 in
         let v2_43=8 in
         (v1_42*v2_43) in
(gcd_29 v_38 v_41)
