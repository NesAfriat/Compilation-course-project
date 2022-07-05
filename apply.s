
set disassembly-flavor intel
layout asm


;at this point we had on stack:
;magic = [rbp+8*(4+n)]
;arg_n-1= [rbp+8*(4+n-1)] ; list
;    ....   
;arg_1= [rbp+8*(4+1)]
;arg_0= [rbp+8*(4+0)] ; function
;arg_count n= [rsp+8*3]
;env_pointer= [rsp+8*2]
;ret adr= [rsp+8*]
; old rbp = [rbp]


apply:
  push rbp
  mov rbp , rsp
  ; now look at stack
  ;anyway-> push magic
  push SOB_NIL_ADDRESS ; push magic

  mov rbx, [rbp+8*4] ; rbx holds the func we wanna apply
  mov rcx,  [rbp+8*3] ;rcx holds argCount(n)
  cmp rcx, 1 ;  
  je .no_arg
  
  ;else-> we have at least the list at the end
  ;push list args
  mov r10, [rbp+8*(4+rcx-1)] ; r10->lst
  mov qword r11, 0; r11= size of list
  cmp r10, SOB_NIL_ADDRESS; if list is empty
  je .end_loop_of_pushes
  ; else-> lst !=nill -> so we need to reverse it and than push each var in it
  ;reverse:
  ;first push by order each var in lst to stack:
  mov qword r12, [r10+1] ;r12->car
  mov qword r13, [r10+1+8] ;r13->cdr
  .loop_of_pushes:
    inc r11
    push qword r12
    cmp r13, SOB_NIL_ADDRESS; if cdr is empty
    je .end_loop_of_pushes
    mov qword r12, [r13+1] ;r12->car
    mov qword r13, [r13+1+8] ;r13->cdr
    jmp .loop_of_pushes
  .end_loop_of_pushes:

  ;now: r11= lst size , stack contains all vars.

  shl r11, 3 ; r11 = 8* lst_size
  MALLOC rax, r11 ; allocate size for lst in heap
  shr r11, 3 ; reset r11 =lst_size
  mov r12, 0 ; idx
  .loop_copy_lst_in_reverse:
    cmp r12, r11 
    jge .end_loop_copy_lst_in_reverse ;jump to end if idx >= lst size
    pop qword [rax +8*r12] ; pop arg from stack and put it in lst[i]
    inc r12
    jmp .loop_copy_lst_in_reverse
  .end_loop_copy_lst_in_reverse:
  ; now rax->reversed lst

  ; now push vars of reversed lst:
  mov r12, 0 ; idx
  .loop_push_reveres_lst:
    cmp r12, r11 
    jge .end_loop_push_reveres_lst ;jump to end if idx >= lst size
    push qword [rax +8*r12] ; pop arg from stack and put it in lst[i]
    inc r12
    jmp .loop_push_reveres_lst
  .end_loop_push_reveres_lst:


  cmp rcx, 2
  je .list_only
  ;else-> we have opt args
  .opt_args:
    mov r12, 1 ; idx
    mov r13, rcx
    sub r13, 2    ; r13= idx of arg_n-2
    .loop_push_opts:
      cmp r12, r13 
      jg .end_loop_push_opts ;jump to end if idx > num of opt args
      push qword [rbp+8*(4+r13)] ; push arg from the end
      dec r13
      jmp .loop_push_opts
    .end_loop_push_opts:

    ;all arg on stack:

    sub rcx, 2
    add rcx, r11  ;rcx = argCount(n) -2 + lstSize
    push rcx  ;push n
    jmp .end_apply

  .list_only:
    push r11 ;push n
    jmp .end_apply
  .no_arg:
    push 0

  .end_apply:
    mov r12, qword [rbx+1]
    push qword r12 ; push env of closure that we are gonna call
    call qword [rbx+9]

    add rsp , 8*1 ; pop env
    pop rbx ; pop arg count
    lea rsp , [rsp + 8*rbx]; pop args
    add rsp , 8 ; pop magic, which is sob_nil

  leave
  ret