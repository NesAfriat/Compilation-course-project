#use "semantic-analyser.ml";;
let tag_size=1;;
let string_len_value_size= 8;;
let bool_value_size= 1;;
let char_value_size= 1;;
let int_float_value_size = 8;;
let symbol_pointer_size = 8;;
let pair_car_pointer_size = 8;;
let pair_cdr_pointer_size = 8;;
let vector_len_value_size =8;;
let vector_pointer_size = 8;;

let idx_if = ref 0;;
let idx_or = ref 0;;
let idx_loop= ref 0;;
let idx_end_loop= ref 0;;
let idx_Lcode = ref 0;;
let idx_Lcont = ref 0;;
let idx_loop_shift_stack = ref 0;;
let update_idx idx=
  idx:= 1+ idx.contents;;


let rec rdc_rac s =
    match s with
    | [e] -> ([], e)
    | e :: s ->
       let (rdc, rac) = rdc_rac s
       in (e :: rdc, rac)
    | _ -> raise X_this_should_not_happen;;

(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module type CODE_GEN = sig
  (* This signature assumes the structure of the constants table is
     a list of key-value pairs:
     - The keys are constant values (Sexpr(x) or Void)
     - The values are pairs of:
       * the ofset from the base const_table address in bytes; and
       * a string containing the byte representation (or a sequence of nasm macros)
         of the constant value
     For example: [(Sexpr(Nil), (1, "T_NIL"))]
   *)
  val make_consts_tbl : expr' list -> (sexpr * (int * string)) list

  (* This signature assumes the structure of the fvars table is
     a list of key-value pairs:
     - The keys are the fvar names as strings
     - The values are the offsets from the base fvars_table address in bytes
     For example: [("boolean?", 0)]
   *)  
  val make_fvars_tbl : expr' list -> (string * int) list

  (* If you change the types of the constants and fvars tables, you will have to update
     this signature to match: The first argument is the constants table type, the second 
     argument is the fvars table type, and the third is an expr' that has been annotated 
     by the semantic analyser.
   *)
  val generate : (sexpr * (int * string)) list -> (string * int) list -> expr' -> string
end;;

let  getFreeLst expr =
let rec run expr lst =
    match expr with
    | ScmVar'(VarFree(name)) -> name:: lst
    | ScmBoxSet' (v, e)->(run e lst)
    | ScmIf'(test, than_p, else_p)->(run test (run than_p (run else_p lst)))
    | ScmSeq'(lst_seq)-> (List.fold_left (fun init cur-> run cur init) lst lst_seq)
    | ScmSet'(VarFree(var), expr')->  var:: (run expr' lst)
    | ScmDef'(VarFree(var), expr')->  var:: (run expr' lst)
    | ScmOr'(lst_or)-> (List.fold_left (fun init cur-> run cur init) lst lst_or)
    | ScmLambdaSimple' (params, body)-> run body lst
    | ScmLambdaOpt' (params, opt,body)-> run body lst
    | ScmApplic' (rator, rands) -> (List.fold_left (fun init cur-> run cur init) lst (rator::rands))
    | ScmApplicTP' (rator, rands) -> (List.fold_left (fun init cur-> run cur init) lst (rator::rands))
    |_-> lst
   in  run expr [];;


let  getConstLst expr =
let rec run expr lst =
    match expr with
    | ScmConst' (sexp)-> sexp::lst
    | ScmBoxSet' (v, e)->(run e lst)
    | ScmIf'(test, than_p, else_p)->(run test (run than_p (run else_p lst)))
    | ScmSeq'(lst_seq)-> (List.fold_left (fun init cur-> run cur init) lst lst_seq)
    | ScmSet' (v,e)-> (run e lst)
    | ScmDef'(v,e)-> (run e lst)
    | ScmOr'(lst_or)-> (List.fold_left (fun init cur-> run cur init) lst lst_or)
    | ScmLambdaSimple' (params, body)-> run body lst
    | ScmLambdaOpt' (params, opt,body)-> run body lst
    | ScmApplic' (rator, rands) -> (List.fold_left (fun init cur-> run cur init) lst (rator::rands))
    | ScmApplicTP' (rator, rands) -> (List.fold_left (fun init cur-> run cur init) lst (rator::rands))
    |_-> lst
   in  run expr [];;
  
let rec extabdSingleConstant sexp =
  match sexp with
    |ScmSymbol (name)-> [ScmString(name)] @ [sexp]
    |ScmPair(car, ScmNil)-> (extabdSingleConstant car) @ [sexp] (*remove? *)
    |ScmPair(car, cdr)-> (extabdSingleConstant car) @ (extabdSingleConstant cdr) @ [sexp] (*take care of car, cdr and than add this pair *)
    |ScmVector (lst)-> (List.fold_left (fun init cur -> init @ (extabdSingleConstant cur)) [] lst) @ [sexp]
    |_-> [sexp] ;;

let lst_to_set lst=
(List.fold_left (fun init cur-> if (List.mem cur init) then init else (init @ [cur])) [] lst );;

let computeOffset cur_offset cur_sexp = 
  match cur_sexp with
  | ScmVoid-> cur_offset+tag_size
  | ScmNil-> cur_offset+tag_size
  | ScmBoolean(b)-> cur_offset+ tag_size+bool_value_size
  | ScmSymbol(name) -> cur_offset+ tag_size+symbol_pointer_size
  | ScmPair(car, cdr)-> cur_offset+ tag_size + pair_car_pointer_size+ pair_cdr_pointer_size
  | ScmNumber(ScmRational(first,sec))-> cur_offset+tag_size+ int_float_value_size*2
  | ScmNumber(number)-> cur_offset+tag_size+ int_float_value_size
  | ScmString(str) -> cur_offset+tag_size+ string_len_value_size + (String.length str)
  | ScmChar(c)-> cur_offset+ tag_size+ char_value_size
  | ScmVector(lst) -> cur_offset+ tag_size+ vector_len_value_size + (List.length lst)*vector_pointer_size;;


let rec getOffset to_find tbl = 
  match tbl with
  |(se,(os, str))::rest-> if (se=to_find) then os else (getOffset to_find rest)
  |_-> -1
  ;;  
let create_vector_asm_code lst tbl=
let str=  List.fold_left (fun init cur-> Printf.sprintf "%sconst_tbl+%d, " init (getOffset cur tbl)) "" lst in
let str=  (if ((String.length str)=0) then str else (String.sub str 0 ((String.length str)-2))) in str;;

let createRow sexp offset table=
  match sexp with
  | ScmVoid-> (sexp, (offset, "db T_VOID"))
  | ScmNil->(sexp, (offset, "db T_NIL"))

  | ScmBoolean(b)-> (if b then (sexp, (offset,  "db T_BOOL ,1")) else (sexp, (offset,  "db T_BOOL ,0")))
  | ScmSymbol(name) -> (sexp, (offset,(Printf.sprintf "MAKE_LITERAL_SYMBOL(const_tbl+%d)" (getOffset (ScmString(name)) table) )))
  | ScmPair(car, cdr)->(sexp, (offset,(Printf.sprintf "MAKE_LITERAL_PAIR(const_tbl+%d, const_tbl+%d)" (getOffset car table) (getOffset cdr table) ) ))

  | ScmNumber(ScmRational(n,d))->(sexp, (offset, (Printf.sprintf "MAKE_LITERAL_RATIONAL(%d, %d)" n d)))
  | ScmNumber(ScmReal(n))-> (sexp, (offset, (Printf.sprintf "MAKE_LITERAL_FLOAT(%f)" n)))
  | ScmString(str) ->(sexp ,(offset , "MAKE_LITERAL_STRING \""^str^"\""))
  | ScmChar(c)-> (sexp, (offset,(Printf.sprintf "MAKE_LITERAL_CHAR(%s)" (string_of_int (int_of_char c)))))
  | ScmVector(lst)-> (sexp, (offset,(Printf.sprintf "MAKE_LITERAL_VECTOR %s"  (create_vector_asm_code lst table))))
  ;;

let createTable sexprs=
  let rec run sexprs offset tbl =
    match sexprs with
      |first::rest-> (run rest (computeOffset offset first) (tbl @ [(createRow first offset tbl)]))
      |_->tbl
  in (run sexprs 0 []);;

let const asts = 
  (*frstLst is the initial table: contains each sexp that was wrapd with scmConst, without duplicates for efficency*)
    let frstLst = List.fold_left (fun init cur -> init @ (getConstLst cur) ) [] asts  in
    let secLst = List.fold_left (fun init cur -> init @ (extabdSingleConstant cur) ) [] frstLst  in
    let thirdLst = [ScmVoid ;ScmNil; ScmBoolean(false); ScmBoolean(true)] @ secLst in
    let forthLst = (lst_to_set thirdLst) in 
    let fifth = createTable forthLst in fifth;;

let createFree sexprs =
 let rec run sexprs idx tbl=
    match sexprs with
      |first::rest-> (run rest (idx+1) (tbl @[(first, idx)]))
      |_->tbl
  in (run sexprs 0 []);;

module Code_Gen : CODE_GEN = struct
  let make_consts_tbl asts = 
  (*frstLst is the initial table: contains each sexp that was wrapd with scmConst, without duplicates for efficency*)
    let frstLst = List.fold_left (fun init cur -> init @ (getConstLst cur) ) [] asts  in
    let secLst = List.fold_left (fun init cur -> init @ (extabdSingleConstant cur) ) [] frstLst  in
    let thirdLst = [ScmVoid ;ScmNil; ScmBoolean(false); ScmBoolean(true)] @ secLst in
    let forthLst = (lst_to_set thirdLst) in 
    let fifth = createTable forthLst in fifth;;

  let make_fvars_tbl asts = 
     let frstLst = List.fold_left (fun init cur -> init @ (getFreeLst cur) ) [] asts  in
     let secLst = ["boolean?"; "flonum?"; "rational?"; "pair?"; "null?"; "char?"; "string?";
    "procedure?"; "symbol?";"string-length"; "string-ref"; "string-set!"; "make-string"; "symbol->string";
    "char->integer"; "integer->char"; "exact->inexact";"eq?";
    "+"; "*"; "/"; "="; "<"; "numerator"; "denominator"; "gcd";"append";"apply";"car";"cdr";"cons"; "cons*";"denominator";
    "equal?";"fold-left";"fold-right";"integer?";"length";"list";"list?"; "map";"not";"number?";"set-car!";"set-cdr!";
    "string->list"; "zero?"] @ frstLst in
    let thirdLst = (lst_to_set secLst) in 
    let withIndexes= (createFree thirdLst) in  withIndexes



  (*-----------generating fuctions--------------*)


  let blue_part params body depth=
  let x= (update_idx idx_end_loop)in
  let x= (update_idx idx_loop)in
  let extendEnv_1 = (Printf.sprintf
";we know that env = [rbp + 8*2]
; we know that depth is env.length
; now we SHOULD allocate extenv with size env.length+1= depth+1
MALLOC rax, 8*(1+%d) ; rax->extEnv
mov rbx, 0 ;i
mov rcx, 1 ;j
;following loop is to shift the env in extended env
loop_%d:
    cmp rbx, %d
    jge end_loop_%d
    mov rdx, [rbp + 8*2] ; rdx = env
    shl rbx, 3
    add rdx, rbx ; env[i] =-->[var, var,...var] 
    shr rbx, 3
    mov r8, [rdx] 
    mov [rax + 8*rcx], r8 ; extEnv[j]=env[i]
    inc rcx
    inc rbx
    jmp loop_%d
end_loop_%d:
 ;  rax holds ->extEnv\n"
 depth idx_loop.contents depth idx_end_loop.contents idx_loop.contents idx_end_loop.contents) in
let x= (update_idx idx_end_loop)in
let x= (update_idx idx_loop)in
let extendEnv_2 = (Printf.sprintf
";n= [rbp+8*3]
mov rbx, rax ;rbx holds extEnv
; Allocate ExtEnv[0] to point to a vector where to store the parameters
mov r8, [rbp+8*3] ; r8= n
inc r8; +magic
shl r8, 3
MALLOC r9, r8 ; r9 holds the vector for the params with size 8*n +1 for magic
shr r8, 3
dec r8; - magic
mov rcx, 0 ;i=0
;following loop is to Copy the parameters off of the stack to the extended env:
loop_%d:
    cmp rcx ,r8 ; i<n
    jge end_loop_%d
    mov rdx, [rbp+8*(4+rcx)] ;  rdx= param_i
    mov [r9+8*rcx], rdx ; ExtEnv [0][i]= param_i
    inc rcx
    jmp loop_%d
end_loop_%d:
mov qword [r9+8*rcx], SOB_NIL_ADDRESS; add magic to the end
mov [rbx], r9 ; updte the extEnv[0]-> vector that we created for the params
\n"
  idx_loop.contents idx_end_loop.contents idx_loop.contents idx_end_loop.contents)in
let closure_obj= (Printf.sprintf "MAKE_CLOSURE(rax, rbx, Lcode_%d)\n jmp Lcont_%d\n" idx_Lcode.contents idx_Lcont.contents )in 
let str= extendEnv_1^extendEnv_2^closure_obj in str;;

(*let adjustStackForOpt params opt=
 params.len is the number of args i expected 
[rbp+8*3] =n is the number of args that have been pushed to the stack
*)
let adjustStackForOpt params opt=
    let params_len = (List.length params) in
    let expected_argCount= params_len+1 in
    let str= (Printf.sprintf 
"mov rdx, rsp ; rdx hold cur stack ptr ==> [rdx]= ret_adr etc... (i chose rdx because cons does'nt overide it)
mov r8, [rdx+8*2] ; r8= arg_count 
cmp r8, %d ; compare arg_count to the amount we've expected
jl .no_fix_necessery
; else: we need to collect all exses args to a list
mov r9, SOB_NIL_ADDRESS ; r9 holds curr list
mov r10, r8
sub r10, %d ; r10= lst size= num of opt args
mov r11, r8
dec r11 ; r11 =idx= n-1
.loop_create_lst:
  cmp r11, %d 
  jl .end_loop_create_lst ; end loop if idx<params.lst, which is when we get to the args that arent optional
  mov r13, rsp; r13=sp
  push SOB_NIL_ADDRESS; push magic
  push r9 ; push cdr
  push qword [rdx+8*(3+r11)] ; push arg_idx = car
  push qword 2; argcount= car, cdr
  push qword [rdx+8*1]; env
  call cons
  mov rsp, r13
  mov r9, rax ; r9 hold new lst
  mov r13, [r9+1]; r13=car
  mov r13, [r9+1+8]; r13=cdr
  dec r11 ; idx--
  jmp .loop_create_lst
.end_loop_create_lst:

; now r9 holds list of optional args
mov r13, [rdx+8*(3+r8-1)]; should be last opt arg=3
mov qword [rdx+8*(3+r8-1)], r9 ; set arg_n-1 to be optional
mov qword [rdx+8*2], %d
mov r9, r8  ; r9= n
inc r9  ;r9 = n+1 =3+n-2
shl r9, 3 ;r9= 8*(3+n-2)= idx of last place in stack
mov r11, %d
add r11, 2 
shl r11, 3 ; r11 = 8*(3+ params_len -1) = idx of last mandatory arg = the first element from stack that should be moved up
mov r13, [rdx+r11]; should be 1
; both r11, r9 will be decresed each time by 8
.loop_shift_stack:
  cmp r11, 0
  jl .end_loop_shift_stack
  mov rax, [rdx+r11] ; rax = cur element to move
  mov [rdx+r9], rax ; move to the top of the stack the cur element top down
  sub r9, 8
  sub r11, 8
  jmp .loop_shift_stack
.end_loop_shift_stack:
; now, rdx+r9 holds the last place on stack
add r9, 8
add r9, rdx
mov rsp, r9
.no_fix_necessery:
" expected_argCount params_len params_len expected_argCount params_len ) in str

let fix_the_stack idx= 
 (Printf.sprintf 
  "SHIFT_FRAME_%d:
mov r15, rbp
sub r15, 8 ; r15= adress in stack of first to move up
mov qword r14, [rbp + 8*3] ; r14=n
add r14, 4
shl r14, 3
add r14, rbp ; r14 = rbp+ 8*(4+n) , which is the adress we should overide first
; we are moving top down:
.loop_of_shifts:
  cmp r15, rsp
  jl .end_loop_of_shifts ; we finished
  mov qword r12, [r15]; r12 = element to move
  mov qword [r14], r12 ; move elemnt to top of stack
  sub r14, 8
  sub r15, 8
  jmp .loop_of_shifts
.end_loop_of_shifts:
  add r14, 8
  mov rsp, r14
  mov rbp, r11
  "  idx);;

  let find_index_tbl fvars_tbl name = 
    let rec run tbl =
    match tbl with
      |(varName,index)::rest-> (if (varName = name) then index else (run rest))
      |_-> -1
    in (run fvars_tbl);;


  let generate consts fvars e = 
  let rec gen expr depth = 
    match expr with
      | ScmConst'(c)->  
        let const_row = (List.find (fun (const,( _, _)) -> sexpr_eq const c) consts) in
        let offset = (fun (_, (off, _)) -> off) const_row in
        Printf.sprintf "mov rax, const_tbl+%d\n" offset
      | ScmVar'(var')-> (match var' with
                  | VarFree (name)-> Printf.sprintf "mov rax, [fvar_tbl+ (8* %d)]\n" (find_index_tbl fvars name) 
                  | VarParam (name,index)-> Printf.sprintf "mov rax, qword [rbp + 8 *(4 + %d)]\n" index
                  | VarBound (name,maj,min)-> Printf.sprintf "mov rax, qword [rbp + 8 * 2]\nmov rax, qword [rax + 8 * (%d)]\nmov rax, qword [rax + 8 * (%d)]\n" maj min)
      | ScmBox'(VarParam (name,index))->"MALLOC rax, 8\n" ^ (Printf.sprintf "mov rbx, qword[rbp+ 8*(4+%d)]\n" index) ^ "mov [rax], rbx\n"
      | ScmBoxGet'(var)-> (gen (ScmVar'(var)) depth)^"mov rax, qword [rax]\n"
      | ScmBoxSet' (var,value)-> (gen value depth)^"push rax\n"^(gen (ScmVar'(var)) depth)^"pop qword [rax]\nmov rax, SOB_VOID_ADDRESS\n"
      | ScmIf'(test,dit,dif)->
      let updateLexit= update_idx idx_if in
      let id_x= idx_if.contents in
        (gen test depth) ^
        (Printf.sprintf "cmp rax, SOB_FALSE_ADDRESS\nje Lelse_%d\n" id_x) ^ 
          (gen dit depth) ^ 
        (Printf.sprintf "jmp Lexit_%d\nLelse_%d:\n" id_x id_x) ^ 
          (gen dif depth) ^
        (Printf.sprintf "Lexit_%d:\n" id_x)
      | ScmSeq'(lst_seq)-> (List.fold_left (fun init cur-> init^(gen cur depth)) "" lst_seq)
      | ScmSet'(VarParam(name, minor),expr')-> 
                              (gen expr' depth) ^ (Printf.sprintf "mov qword [rbp + 8 * (4 + (%d))],rax\nmov rax, SOB_VOID_ADDRESS\n" minor)
      | ScmSet' (var,value)-> (
        match var with
          | VarFree (name)-> (gen value depth) ^ 
                (Printf.sprintf "mov qword [fvar_tbl+ (8* %d)], rax ;mov value \n" (find_index_tbl fvars name)) ^
                "mov rax, SOB_VOID_ADDRESS\n"
          | VarParam (name,index)-> (gen value depth) ^ 
                 (Printf.sprintf "mov qword [rbp + 8 *(4 + %d)], rax\n" index)^
                "mov rax, SOB_VOID_ADDRESS\n"
          | VarBound (name,maj,min)-> (gen value depth) ^ 
                 (Printf.sprintf "mov rbx, qword [rbp + 8 * 2]\nmov rbx, qword [rbx + 8 * (%d)]\nmov qword [rbx + 8 * (%d)], rax\n" maj min)^
                "mov rax, SOB_VOID_ADDRESS\n"
      )
      | ScmDef'(VarFree(var), expr')->  
          let varIdx= find_index_tbl fvars var in
          (gen expr' depth) ^ (Printf.sprintf "mov [fvar_tbl+(8*%d)], rax\nmov rax, SOB_VOID_ADDRESS\n" varIdx)
       | ScmOr'(lst)->(
        let x = update_idx idx_or in
        let lable =(Printf.sprintf "Lexit_Or_%d" idx_or.contents) in
        let p_str= "cmp rax, SOB_FALSE_ADDRESS\njne "^lable^"\n" in
        let (frst, last)= rdc_rac lst in 
        let str = (List.fold_left (fun init cur_e ->
            init^((gen cur_e depth)^ p_str)) "" frst) in
        let str = str^(gen last depth)^ lable^":\n" in str
      )
      | ScmLambdaSimple' (params, body)->
        (
          let x= (update_idx idx_Lcont)in
          let x= (update_idx idx_Lcode)in
          let blue= (blue_part params body depth)in
          let cur_idx_cont= idx_Lcont.contents in
          let cur_idx_code= idx_Lcode.contents in
          let str_body= (gen body (depth+1) ) in
          let orange = (Printf.sprintf
          "Lcode_%d:
push rbp
mov rbp , rsp
%s
leave
ret
Lcont_%d:\n"
       cur_idx_code str_body cur_idx_cont ) in blue^orange
       )

      | ScmLambdaOpt' (params,opt, body)->
        (
          let x= (update_idx idx_Lcont)in
          let x= (update_idx idx_Lcode)in
          let blue= (blue_part params body depth)in
           let cur_idx_cont= idx_Lcont.contents in
          let cur_idx_code= idx_Lcode.contents in
          let str_body= (gen body (depth+1) ) in
          let str_adjustStackForOpt =  (adjustStackForOpt  params opt)in
          let orange = (Printf.sprintf
          "Lcode_%d:
%s
push rbp
mov rbp , rsp
%s
leave
ret
Lcont_%d:\n"
       cur_idx_code str_adjustStackForOpt str_body cur_idx_cont ) in blue^orange
       )
      | ScmApplic' (rator,rands)->(
         let magic =";applic:
         push SOB_NIL_ADDRESS ; push magic\n"in
         let pushArgs= (List.fold_left (fun init cur-> init^(gen cur depth)^"push rax ; calculate arg, rax holds arg, and push it\n") "" (List.rev rands))in
         let pushN= (Printf.sprintf "push %d ; push argcount\n;next is proc:\n" (List.length rands)) in
         let proc = (gen rator depth)in
         let prep = "; we back from calculating proc\ncmp byte [rax], T_CLOSURE\njne appError ;verify clousre\n"^
                    "push qword [rax+1] ; push env\n"^
                    "call qword [rax+9] ; call to code ptr\n " in
         let after_return = "add rsp , 8*1 ; pop env\n"^
                  "pop rbx ; pop arg count\n"^
                  "lea rsp , [rsp + 8*rbx]; pop args\n"^
                  "add rsp , 8 ; pop magic, which is sob_nil\n" in
          let result = magic^pushArgs^pushN^proc^prep^after_return in result
      )
      | ScmApplicTP' (rator,rands)->(
         let x = (update_idx idx_loop_shift_stack) in
         let idx= idx_loop_shift_stack.contents in
         let magic =";applic tp:\npush SOB_NIL_ADDRESS ; push magic\n"in
         let pushArgs= (List.fold_left (fun init cur-> init^(gen cur depth)^"push rax ; calculate arg, rax holds arg, and push it\n") "" (List.rev rands))in
         let pushN= (Printf.sprintf "push %d ; push argcount\n;next is proc:\n" (List.length rands)) in
         let proc = (gen rator depth)in
         let prep = "; we back from calculating proc\ncmp byte [rax], T_CLOSURE\njne appError ;verify clousre\n"^
                    "push qword [rax+1] ; push env\n"^
                    "push qword [rbp + 8 * 1] ; old ret addr\n"^
                    "mov r11, qword [rbp]\n" in
         let str_fix_the_stack =(fix_the_stack idx) in
         let jmp_instade_of_call = "jmp qword [rax+9] ; jump to code ptr\n " in
          let result = magic^pushArgs^pushN^proc^prep^str_fix_the_stack^jmp_instade_of_call in result
      )
      |_-> raise X_this_should_not_happen
      in (gen e 0)
   

end;;


