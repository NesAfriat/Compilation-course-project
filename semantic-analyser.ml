(* semantic-analyser.ml
 * The semantic analysis phase of the compiler
 *
 * Programmer: Mayer Goldberg, 2021
 *)

#use "tag-parser.ml";;

exception X_not_yet_implemented;;
exception X_this_should_not_happen;;

type var' = 
  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int;;

type expr' =
  | ScmConst' of sexpr
  | ScmVar' of var'
  | ScmBox' of var'
  | ScmBoxGet' of var'
  | ScmBoxSet' of var' * expr'
  | ScmIf' of expr' * expr' * expr'
  | ScmSeq' of expr' list
  | ScmSet' of var' * expr'
  | ScmDef' of var' * expr'
  | ScmOr' of expr' list
  | ScmLambdaSimple' of string list * expr'
  | ScmLambdaOpt' of string list * string * expr'
  | ScmApplic' of expr' * (expr' list)
  | ScmApplicTP' of expr' * (expr' list);;


let var_eq v1 v2 =
match v1, v2 with
  | VarFree (name1), VarFree (name2) -> String.equal name1 name2
  | VarBound (name1, major1, minor1), VarBound (name2, major2, minor2) ->
    major1 = major2 && minor1 = minor2 && (String.equal name1 name2)
  | VarParam (name1, index1), VarParam (name2, index2) ->
       index1 = index2 && (String.equal name1 name2)
  | _ -> false

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | ScmConst' (sexpr1), ScmConst' (sexpr2) -> sexpr_eq sexpr1 sexpr2
  | ScmVar' (var1), ScmVar' (var2) -> var_eq var1 var2
  | ScmIf' (test1, dit1, dif1), ScmIf' (test2, dit2, dif2) -> (expr'_eq test1 test2) &&
                                            (expr'_eq dit1 dit2) &&
                                              (expr'_eq dif1 dif2)
  | (ScmSeq' (exprs1), ScmSeq' (exprs2) | ScmOr' (exprs1), ScmOr' (exprs2)) ->
        List.for_all2 expr'_eq exprs1 exprs2
  | (ScmSet' (var1, val1), ScmSet' (var2, val2) | ScmDef' (var1, val1), ScmDef' (var2, val2)) ->
        (var_eq var1 var2) && (expr'_eq val1 val2)
  | ScmLambdaSimple' (vars1, body1), ScmLambdaSimple' (vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) && (expr'_eq body1 body2)
  | ScmLambdaOpt' (vars1, var1, body1), ScmLambdaOpt' (vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) && (expr'_eq body1 body2)
  | ScmApplic' (e1, args1), ScmApplic' (e2, args2) ->
     (expr'_eq e1 e2) && (List.for_all2 expr'_eq args1 args2)
  | ScmApplicTP' (e1, args1), ScmApplicTP' (e2, args2) ->
      (expr'_eq e1 e2) && (List.for_all2 expr'_eq args1 args2)
  | ScmBox' (v1), ScmBox' (v2) -> var_eq v1 v2
  | ScmBoxGet' (v1), ScmBoxGet' (v2) -> var_eq v1 v2
  | ScmBoxSet' (v1, e1), ScmBoxSet' (v2, e2) -> (var_eq v1 v2) && (expr'_eq e1 e2)
  | _ -> false;;


module type SEMANTIC_ANALYSIS = sig
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
  val run_semantics : expr -> expr'
end;; (* end of module type SEMANTIC_ANALYSIS *)

module Semantic_Analysis 
(*:SEMANTIC_ANALYSIS*) = struct

  let rec lookup_in_rib name = function
    | [] -> None
    | name' :: rib ->
       if name = name'
       then Some(0)
       else (match (lookup_in_rib name rib) with
             | None -> None
             | Some minor -> Some (minor + 1));;

  let rec lookup_in_env name = function
    | [] -> None
    | rib :: env ->
       (match (lookup_in_rib name rib) with
        | None ->
           (match (lookup_in_env name env) with
            | None -> None
            | Some(major, minor) -> Some(major + 1, minor))
        | Some minor -> Some(0, minor));;

  let tag_lexical_address_for_var name params env = 
    match (lookup_in_rib name params) with
    | None ->
       (match (lookup_in_env name env) with
        | None -> VarFree name
        | Some(major, minor) -> VarBound(name, major, minor))
    | Some minor -> VarParam(name, minor);;

  (* run this first! *)
  let annotate_lexical_addresses pe = 
   let rec run pe params env =
      match pe with
      |ScmLambdaSimple (params_lst,body)->
        ScmLambdaSimple'(params_lst, (run body params_lst (List.append [params] env)))
      |ScmLambdaOpt ( params_lst, opt ,body)->
        ScmLambdaOpt'(params_lst,opt, 
                      (run body (List.append params_lst [opt]) (List.append [params] env )))
      |ScmApplic(operator, operands)-> 
          ScmApplic'((run operator params env), (List.map (fun rand->run rand params env) operands))
      |ScmIf(test, then_p, else_p)-> 
          ScmIf'((run test params env), (run then_p params env), (run else_p params env))
      |ScmSeq(lst)->ScmSeq'( (List.map (fun exp->run exp params env) lst))
      |ScmSet(ScmVar(v), expr)->ScmSet'( (tag_lexical_address_for_var v params env),(run expr params env))
      |ScmDef (ScmVar(var),expr)-> ScmDef'(VarFree(var), (run expr params env))
      |ScmOr(lst)->ScmOr'( (List.map (fun exp->run exp params env) lst))
      |ScmVar (x)-> ScmVar'(tag_lexical_address_for_var x params env)
      |ScmConst(exp)->ScmConst'(exp)
       in 
       run pe [] [];;

  let rec rdc_rac s =
    match s with
    | [e] -> ([], e)
    | e :: s ->
       let (rdc, rac) = rdc_rac s
       in (e :: rdc, rac)
    | _ -> raise X_this_should_not_happen;;
  
  (* run this second! *)
 let annotate_tail_calls pe =
   let rec run pe in_tail =
      match pe with
        | ScmIf' (test, t_p, e_p)->
          (if (in_tail) 
          then (ScmIf'((run test false),(run t_p true), (run e_p true)))
          else (ScmIf'((run test false),(run t_p false), (run e_p false))))
        | ScmSeq' (lst)->
            (if (in_tail) 
            then (
              let (lst, last)= (rdc_rac lst) in
              let res= ScmSeq' (List.append 
              (List.map (fun exp-> run exp false) lst) [(run last true)] ) in res)
            else (ScmSeq' (List.map (fun exp-> run exp in_tail) lst))) 
        | ScmSet'(var, exp)-> ScmSet'(var, (run exp false))
        | ScmDef' (var, exp)-> ScmDef'(var, (run exp in_tail))
        | ScmOr'(lst)->
          (if (in_tail) 
          then (
            let (lst, last)= (rdc_rac lst) in
            let res= ScmOr' (List.append 
            (List.map (fun exp-> run exp false) lst) [(run last true)] )in res)
           else (ScmOr' (List.map (fun exp-> run exp in_tail) lst))) 
        | ScmLambdaSimple' (params, body)-> ScmLambdaSimple'(params,(run body true))
        | ScmLambdaOpt' (params, opt, body)-> ScmLambdaOpt' (params, opt, (run body true))
        | ScmApplic' (operator, operands)->
          (if (in_tail) 
          then (ScmApplicTP' ((run operator false),(List.map (fun rand-> run rand false ) operands)) )
          else (ScmApplic' ((run operator in_tail),(List.map (fun rand-> run rand in_tail) operands)) ))
        |_->pe

   in 
   run pe false;;
  (* boxing *)
 let find_reads name enclosing_lambda expr = 
  let rec run name enclosing_lambda expr change_el lst =
    match expr with
    |ScmVar'(varT)-> (match varT with
                      | VarParam (n, idx)->if n=name then enclosing_lambda::lst else lst
                      | VarBound (n, idxE, isxP)-> if n=name then enclosing_lambda::lst else lst
                      |_-> lst
                     )
    |ScmLambdaSimple' (params, body)->(
        if (List.exists (fun p-> p=name) params) 
        then lst
        else (
          if change_el 
          then (run name expr body false lst )
          else (run name enclosing_lambda body change_el lst )))
    | ScmLambdaOpt'(params, opt, body)-> (
        if ((List.exists (fun p-> p=name) params ) || (opt=name))
        then lst
        else (
          if change_el 
          then (run name expr body false lst)
          else (run name enclosing_lambda body change_el lst )))
    | ScmIf'(test , t_p, e_p)-> 
      (run name enclosing_lambda test change_el (run name enclosing_lambda t_p change_el (run name enclosing_lambda e_p change_el lst)))
    | ScmSeq'(expsL)-> (List.fold_left (fun init cur-> run name enclosing_lambda cur change_el init) lst expsL)
    | ScmSet'(var, value) ->(run name enclosing_lambda value change_el lst)
    | ScmBoxSet'(var, value)-> (run name enclosing_lambda value change_el lst)
    | ScmDef' (var, value) ->(run name enclosing_lambda value change_el lst)
    | ScmOr' (expsL)-> (List.fold_left (fun init cur-> run name enclosing_lambda cur change_el init) lst expsL)
    | ScmApplic'(rator, rands)->(List.fold_left (fun init cur-> run name enclosing_lambda cur change_el init) lst (rator::rands))
    | ScmApplicTP' (rator, rands)-> (List.fold_left (fun init cur-> run name enclosing_lambda cur change_el init) lst (rator::rands))
    | _-> lst
   in run name enclosing_lambda expr true [];;

 let rec getMinor name lst= 
    match lst with
    | name' :: rib ->
      if (name'=name) then 0 else (1 + (getMinor name rib))
    |_-> -1 ;;

 let find_writes name enclosing_lambda expr = 
   let rec run name enclosing_lambda expr change_el lst =
    match expr with
    |ScmLambdaSimple' (params, body)->(
        if (List.exists (fun p-> p=name) params) 
        then lst
        else (
          if change_el 
          then (run name expr body false lst )
          else (run name enclosing_lambda body change_el lst )))
    | ScmLambdaOpt'(params, opt, body)-> (
        if ((List.exists (fun p-> p=name) params ) || (opt=name))
        then lst
        else (
          if change_el 
          then (run name expr body false lst)
          else (run name enclosing_lambda body change_el lst )))
    | ScmIf'(test , t_p, e_p)-> 
      (run name enclosing_lambda test change_el (run name enclosing_lambda t_p change_el (run name enclosing_lambda e_p change_el lst)))
    | ScmSeq'(expsL)-> 
    (List.fold_left (fun init cur-> run name enclosing_lambda cur change_el init) lst expsL)
    | ScmSet'(var, value) -> (match var with
                      | VarParam (n, idx)-> if n=name
                                            then(run name enclosing_lambda value change_el (enclosing_lambda::lst))
                                            else (run name enclosing_lambda value change_el lst)
                      | VarBound (n, idxE, isxP)-> if n=name
                                            then(run name enclosing_lambda value change_el (enclosing_lambda::lst))
                                            else (run name enclosing_lambda value change_el lst)
                      |_-> (run name enclosing_lambda value change_el lst)
                     )
    | ScmBoxSet'(var, value) -> (run name enclosing_lambda value change_el lst)
    | ScmDef' (var, value) ->(run name enclosing_lambda value change_el lst)
    | ScmOr' (expsL)-> (List.fold_left (fun init cur-> run name enclosing_lambda cur change_el init) lst expsL)
    | ScmApplic'(rator, rands)->(List.fold_left (fun init cur-> run name enclosing_lambda cur change_el init) lst (rator::rands))
    | ScmApplicTP' (rator, rands)-> (List.fold_left (fun init cur-> run name enclosing_lambda cur change_el init) lst (rator::rands))
    | _-> lst
   in run name enclosing_lambda expr true [];;

  let rec rappedWithBox name expr = 
    match expr with
    |ScmVar'(varT)-> (match varT with
                      | VarParam (n, idx)-> if n=name then ScmBoxGet'(varT) else expr
                      | VarBound (n, idxE, isxP)->  if n=name then ScmBoxGet'(varT) else expr
                      |_-> expr
                     )
    |ScmLambdaSimple' (params, body)->(
        if (List.exists (fun p-> p=name) params) 
        then expr
        else (ScmLambdaSimple'( params, (rappedWithBox name body)))
        )
    | ScmLambdaOpt'(params, opt, body)-> (
        if ((List.exists (fun p-> p=name) params ) || (opt=name))
        then expr
        else (ScmLambdaOpt'( params,opt, (rappedWithBox name body)))
        )
    | ScmIf'(test , t_p, e_p)-> ScmIf'((rappedWithBox name test) ,(rappedWithBox name t_p), (rappedWithBox name e_p))
    | ScmSeq'(expsL)-> ScmSeq'(List.map (fun cur-> rappedWithBox name cur) expsL)
    | ScmSet'(var, value) ->(match var with
                      | VarParam (n, idx)-> if n=name
                                           then ScmBoxSet'(var, (rappedWithBox name value) ) 
                                           else ScmSet'( var, (rappedWithBox name value)) 
                      | VarBound (n, idxE, isxP)-> if n=name
                                           then ScmBoxSet'(var, (rappedWithBox name value) ) 
                                           else ScmSet'( var, (rappedWithBox name value)) 
                      |_-> ScmSet'( var, (rappedWithBox name value))
                     ) 
    | ScmDef' (var, value) ->ScmDef'( var, (rappedWithBox name value) )
    | ScmOr' (expsL)-> ScmOr'(List.map (fun cur-> rappedWithBox name cur) expsL)
    | ScmApplic'(rator, rands)-> ScmApplic'((rappedWithBox name rator), (List.map (fun cur-> rappedWithBox name cur) rands))
    | ScmApplicTP' (rator, rands)-> ScmApplicTP'((rappedWithBox name rator), (List.map (fun cur-> rappedWithBox name cur) rands))
    | _-> expr;;

  
  
  let mainBox name enclosing_lambda body minor =
    let readsLST = (find_reads name enclosing_lambda body) in
    let writesLST = (find_writes name enclosing_lambda body) in
    let res =(
      if ((List.length readsLST) = 0  || (List.length writesLST) = 0) 
      then body
      else (
        if( (List.exists (fun rl-> (List.for_all (fun wl-> wl<>rl) writesLST)) readsLST) ||
            (List.exists (fun rl-> (List.for_all (fun wl-> wl<>rl) readsLST)) writesLST )
          )
        then (
          match (rappedWithBox name body) with
          |ScmSeq'(lst)-> ScmSeq'((ScmSet'(VarParam(name, minor), ScmBox'(VarParam(name,minor))))::lst)
          |x-> ScmSeq'((ScmSet'(VarParam(name, minor), ScmBox'(VarParam(name,minor))))::[x])
        )
         else (
           if( ((List.length readsLST) = (List.length writesLST)) && (List.length readsLST)>1 )
           then (
            match (rappedWithBox name body) with
            |ScmSeq'(lst)-> ScmSeq'((ScmSet'(VarParam(name, minor), ScmBox'(VarParam(name,minor))))::lst)
            |x-> ScmSeq'((ScmSet'(VarParam(name, minor), ScmBox'(VarParam(name,minor))))::[x])
          )
          else body
           
        )
      )
    ) in res;;

  let rec box_set expr =  
    match expr with
    | ScmIf'(test , t_p, e_p)-> ScmIf'((box_set test) , (box_set t_p), (box_set e_p))
    | ScmSeq'(lst)-> ScmSeq'(List.map (fun exp-> box_set exp) lst)
    | ScmSet'(var, value) ->ScmSet'(var, (box_set value))
    | ScmBoxSet'(var, value)->ScmBoxSet'(var, (box_set value))
    | ScmDef' (var, value) ->ScmDef'(var, (box_set value))
    | ScmOr' (lst)->ScmOr'(List.map (fun exp-> box_set exp) lst)
    | ScmLambdaSimple' (params, body)-> ScmLambdaSimple' (params, 
         (box_set (List.fold_right (fun curP init-> mainBox curP expr init (getMinor curP params)) params body ))
        )
    | ScmLambdaOpt'(params, opt, body)-> 
        ScmLambdaOpt'(params, opt,
          (List.fold_right (fun curP init-> mainBox curP expr init (getMinor curP (opt::params))) (opt::params) body )
        )
    | ScmApplic'(rator, rands)-> ScmApplic'((box_set rator),(List.map (fun exp-> box_set exp) rands))
    | ScmApplicTP' (rator, rands)->ScmApplicTP'((box_set rator),(List.map (fun exp-> box_set exp) rands))
    | _-> expr


  let run_semantics expr =
    box_set
      (annotate_tail_calls
         (annotate_lexical_addresses expr))

end;; (* end of module Semantic_Analysis *)
