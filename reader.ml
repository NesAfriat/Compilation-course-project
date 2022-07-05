(* reader.ml
 * A skeleton for the reader for the 2021-2022 course on compiler-construction
 *)

#use "pc.ml";;

let rec gcd a b =
  match (a, b) with
  | (0, b) -> b
  | (a, 0) -> a
  | (a, b) -> gcd b (a mod b);;

type scm_number =
  | ScmRational of (int * int)
  | ScmReal of float;;

type sexpr =
  | ScmVoid
  | ScmNil
  | ScmBoolean of bool
  | ScmChar of char
  | ScmString of string
  | ScmSymbol of string
  | ScmNumber of scm_number
  | ScmVector of (sexpr list)
  | ScmPair of (sexpr * sexpr);;

module type READER = sig
    val nt_sexpr : sexpr PC.parser
end;; (* end of READER signature *)

module Reader : READER = struct
open PC;;

let unitify nt = pack nt (fun _ -> ());;
let digit = range '0' '9';;
let lowLatter = range 'a' 'z';;
let upLatter = range 'A' 'Z';;
let hexletter = range 'a' 'f';;
let natural= plus digit;;
let sign_optional = maybe (disj (char '+') (char '-'));;



let rec nt_whitespace str =
  const (fun ch -> ch <= ' ') str
and nt_end_of_line_or_file str =
  let nt1 = unitify (char '\n') in
  let nt2 = unitify nt_end_of_input in
  let nt1 = disj nt1 nt2 in
  nt1 str
and nt_boolean str =
  let nt1 = word_ci "#t" in
  let nt1 = pack nt1 (fun _-> true) in
  let nt2 = word_ci "#f" in
  let nt2 = pack nt2 (fun _-> false) in
  let nt1 = disj nt1 nt2 in 
  let nt1 = not_followed_by nt1 nt_symbol_char in
  let nt1 = pack nt1 (fun b-> ScmBoolean b) in nt1 str  
and nt_line_comment str = 
  let nt_end= disj (unitify (char '\n')) (unitify nt_end_of_input) in
  let nt1= char ';' in 
  let nt2 = diff nt_any nt_end in 
  let nt2= star nt2 in
  let nt1 = caten nt1 (caten nt2 nt_end) in 
  let nt1 = unitify nt1 in nt1 str
  and nt_paired_comment str = 
   let lparen= char '{' in 
  let rparen = char '}' in 
  let group1 = disj_list [(unitify nt_char);
                      (unitify nt_string);
                      (unitify nt_symbol);
                      nt_comment] in
  let parens= unitify (disj lparen rparen) in
  let parens_and_g1 = disj group1 parens in
  let group2 = unitify (diff nt_any parens_and_g1) in
  let exp  =star (disj group1 group2)  in
  let nt_final = unitify(caten lparen (caten exp rparen)) 
    in nt_final str
and nt_sexpr_comment str = 
  let nt_start= word "#;" in 
  let nt1 = caten nt_start nt_sexpr in 
  let nt1 = unitify nt1 in nt1 str
and nt_comment str =
  disj_list
    [nt_line_comment;
     nt_paired_comment;
     nt_sexpr_comment] str
and nt_skip_star str =
  let nt1 = disj (unitify nt_whitespace) nt_comment in
  let nt1 = unitify (star nt1) in
  nt1 str
and make_skipped_star (nt : 'a parser) =
  let nt1 = caten nt_skip_star (caten nt nt_skip_star) in
  let nt1 = pack nt1 (fun (_, (e, _)) -> e) in
  nt1
and nt_int str = 
let number= caten sign_optional natural in
  let nt1 =pack number (fun (sign_optional,absNumber)-> match sign_optional with
      |None -> (int_of_string (list_to_string absNumber))
      |Some '-' -> (int_of_string (list_to_string absNumber)) * -1
      |_->(int_of_string (list_to_string absNumber))) in nt1 str
and nt_frac str =   
  let div_op= char '/' in  
  let nat = diff natural (word "0") in
  let nt0= pack nat (fun lst->int_of_string (list_to_string lst)) in 
  let nt1= caten nt_int (caten div_op nt0) in  
  let nt1 = pack nt1 (fun (num1,(_, num2)) ->let ans= gcd num1 num2 in
  ScmRational(num1/ans,num2/ans))  in nt1 str
and nt_integer_part str = 
  let nt1 = natural in nt1 str
and nt_mantissa str = 
  let nt1 = natural in nt1 str
and nt_exponent str = 
  let nt1= word_ci "e" in
  let nt2= word "*10^" in
  let nt3= word "*10**" in
 let expToken= disj_list [nt1; nt2 ;nt3] in
 let nt0 = caten expToken nt_int in nt0 str
and nt_float str =
  let _pt= char '.' in
  let _eMaker strE= match strE with
      |None -> ("")
      |Some (_,_int) -> (String.concat "" ["e";(string_of_int _int)]) in
      
  let floatA= caten nt_integer_part (caten _pt (caten (maybe nt_mantissa) (maybe nt_exponent))) in
   let floatA = pack floatA (fun(iP,(pt,(mant,exp)))->match mant with
      |None -> (float_of_string (String.concat "" [(list_to_string iP);"." ;(_eMaker exp)]))
      |Some mant-> (float_of_string (String.concat "" [(list_to_string iP); "." ; (list_to_string mant); (_eMaker exp)]))) in
  let floatB= caten _pt (caten nt_mantissa (maybe nt_exponent)) in
    let floatB = pack floatB (fun (pt,(mant,exp))->
     (float_of_string ((String.concat "" ["." ;(list_to_string mant);( _eMaker exp)])))) in
  let floatC = caten nt_integer_part nt_exponent in
   let floatC = pack floatC (fun (iP,exp)-> 
     (float_of_string (String.concat "" [(list_to_string iP);"." ; "e" ;(fun (ls,num)->(string_of_int num))exp]))) in
  let floats= disj_list[floatA;floatB;floatC] in 
  let float= caten sign_optional floats in 
  let nt1 = pack float (fun (sign_optional,fNumber)-> match sign_optional with
      |None -> ( ScmReal(fNumber))
      |Some '-' -> ( ScmReal(fNumber *. (-1.0)))
      |_->( ScmReal(fNumber))) in nt1 str
   and nt_number str =
  let nt1 = nt_float in
  let nt2 = nt_frac in
  let nt3 = pack nt_int (fun n -> ScmRational(n, 1)) in
  let nt1 = disj nt1 (disj nt2 nt3) in
  let nt1 = pack nt1 (fun r -> ScmNumber r) in
  let nt1 = not_followed_by nt1 nt_symbol_char in
  nt1 str
and nt_char_simple str = 
  let nt1= range '!' '~' in nt1 str 
and make_named_char char_name ch =  
  let nt0 = word_ci char_name in 
  let nt0 = pack nt0 (fun(_)-> ch) in nt0 
and nt_char_named str =
  let nt1 = 
    disj_list [(make_named_char "newline" '\n');
               (make_named_char "page" '\012');
               (make_named_char "return" '\r');
               (make_named_char "space" ' ');
               (make_named_char "nul" '\000');
               (make_named_char "tab" '\t')] in
  nt1 str
and nt_char_hex str =
  let nt1= disj_list [digit; hexletter] in nt1 str 

and nt_HexadecimalChar str=
  let pref = char 'x' in
  let nt1 = caten pref (plus nt_char_hex) in
  let nt1 = pack nt1 (fun (_,ls)-> 
  (char_of_int (int_of_string (String.concat "" [ "0x"; (list_to_string ls)])))) in nt1 str
and nt_char str = 
  let _charPref= word "#\\" in
  let nt1 = caten _charPref (disj_list [ nt_HexadecimalChar; nt_char_named; nt_char_simple]) in 
  let nt1 = not_followed_by nt1 nt_symbol_char in 
  let nt1 = pack nt1 (fun(pre,ch)->ScmChar(ch)) in nt1 str
and nt_symbol_char str = 
  let c1= char '$' in
  let c2= char '^' in
  let c3= char '*' in
  let c4= char '-' in
  let c5= char '_' in
  let c6= char '=' in
  let c7= char '+' in
  let c8= char '<' in
  let c9= char '>' in
  let c10= char '?' in
  let c11= char '/' in
  let c12= char ':' in
  let c13= char '!' in
  let nt1= disj_list [c1; c2 ;c3; c4; c5; c6; c7; c8; c9; c10; c11; c12 ; c13] in
  let nt2= disj_list [digit; lowLatter ;upLatter] in
  let nt3= disj nt1 nt2  in nt3 str 
and nt_symbol str =
  let nt1 = plus nt_symbol_char in
  let nt1 = pack nt1 list_to_string in
  let nt1 = pack nt1 (fun name -> ScmSymbol name) in
  let nt1 = diff nt1 nt_number in
  nt1 str
and nt_str_LiteralChar str =  
let all_chars = nt_any in
let cn= disj_list [(char '\\') ; (char '~'); (char '\"')] in
let nt1 = diff all_chars cn in nt1 str
and nt_str_MetaChar str =
let nt1 = disj_list [(make_named_char "\\\\" '\\');(make_named_char "\\\"" '\"') ;(make_named_char "\\t" '\t'); (make_named_char "\\f" '\012');
(make_named_char "\\n" '\n'); (make_named_char "\\r" '\r'); (make_named_char "~~" '~')] in nt1 str
and nt_StringHexChar str=
let pref = word "\\x" in
let nt1 = caten pref (caten (plus nt_char_hex) (char ';')) in
let nt1 = pack nt1 (fun (_,(sec,_))-> 
  (char_of_int (int_of_string (String.concat "" [ "0x"; (list_to_string sec)]))))
in nt1 str
and nt_InterChar str= 
  let pref= word "~{" in
  let rparen= char '}' in 
  let nt1 = caten pref (caten nt_sexpr rparen) in
  let nt1 = pack nt1 (fun(_,(sexpr,_))-> 
    ScmPair(ScmSymbol("format"),ScmPair(ScmString("~a"),ScmPair(sexpr,ScmNil)))) 
  in nt1 str
  and nt_string str = 
  let stati = plus (disj_list [nt_str_MetaChar; nt_str_LiteralChar; nt_StringHexChar ] )in
  let stati = pack stati (fun (lst)-> ScmString(list_to_string lst)) in
  let dyn = nt_InterChar in 
  let types = disj stati dyn in 
  let nt1 = caten (char '\"') (caten (star types) (char '\"')) in 
  let nt1 = pack nt1 (fun(qml,(sexp_lst,qmr))->
       match (List.length sexp_lst) with
       | 0 -> ScmString("")
       | 1 -> List.hd sexp_lst
       | _-> ScmPair ((ScmSymbol "string-append"),
                      (List.fold_right (fun cur acc-> ScmPair(cur, acc)) sexp_lst ScmNil)))
       in nt1 str
and nt_vector str =
  let nt1 = word "#(" in
  let nt2 = caten nt_skip_star (char ')') in
  let nt2 = pack nt2 (fun _ -> ScmVector []) in
  let nt3 = plus nt_sexpr in
  let nt4 = char ')' in
  let nt3 = caten nt3 nt4 in
  let nt3 = pack nt3 (fun (sexprs, _) -> ScmVector sexprs) in
  let nt2 = disj nt2 nt3 in
  let nt1 = caten nt1 nt2 in
  let nt1 = pack nt1 (fun (_, sexpr) -> sexpr) in
  nt1 str
and nt_list str = 
let _lrp = char '(' in
let _rpr = caten nt_skip_star (char ')') in
let _impList= caten (plus nt_sexpr) (caten (char '.') nt_sexpr) in
let _impList = pack _impList (fun(ls1,(dot,ls2))-> (List.fold_right (fun cur acc-> ScmPair(cur, acc)) ls1 ls2)) in
let _impList = caten _lrp (caten _impList _rpr) in
let _pList= pack (star nt_sexpr) (fun(ls1)->List.fold_right (fun cur acc-> ScmPair(cur, acc)) ls1 ScmNil) in 
let _pList = caten _lrp (caten _pList _rpr) in
let nt1= (disj_list [_pList; _impList]) in 
let nt1 = pack nt1 (fun(lpar,(sexp,rpar))-> sexp) in nt1 str
and nt_quoted_forms str = 
let _quote = caten (char '\'') nt_sexpr in
let _quote = pack _quote (fun(q,se)-> ScmPair(ScmSymbol("quote"),ScmPair(se,ScmNil))) in
let _qquote= caten (char '`') nt_sexpr in
let _qquote = pack _qquote (fun(qq,se)-> ScmPair(ScmSymbol("quasiquote"),ScmPair(se,ScmNil))) in
let _unquote= caten (char ',') nt_sexpr in
let _unquote = pack _unquote (fun(uq,se)-> ScmPair(ScmSymbol("unquote"),ScmPair(se,ScmNil))) in
let _unquotesplice= caten (word ",@") nt_sexpr in
let _unquotesplice = pack _unquotesplice (fun(uqsp,se)-> ScmPair(ScmSymbol("unquote-splicing"),ScmPair(se,ScmNil))) in
let nt1 = disj_list [_quote;_qquote;_unquote;_unquotesplice] in nt1 str
and nt_sexpr str =
  let nt1 =
    disj_list [nt_number; nt_boolean; nt_char; nt_symbol;
               nt_string; nt_vector; nt_list;nt_quoted_forms] in
  let nt1 = make_skipped_star nt1 in
  nt1 str;;

end;; (* end of struct Reader *)

let rec string_of_sexpr = function
  | ScmVoid -> "#<void>"
  | ScmNil -> "()"
  | ScmBoolean(false) -> "#f"
  | ScmBoolean(true) -> "#t"
  | ScmChar('\n') -> "#\\newline"
  | ScmChar('\r') -> "#\\return"
  | ScmChar('\012') -> "#\\page"
  | ScmChar('\t') -> "#\\tab"
  | ScmChar(' ') -> "#\\space"
  | ScmChar(ch) ->
     if (ch < ' ')
     then let n = int_of_char ch in
          Printf.sprintf "#\\x%x" n
     else Printf.sprintf "#\\%c" ch
  | ScmString(str) ->
     Printf.sprintf "\"%s\""
       (String.concat ""
          (List.map
             (function
              | '\n' -> "\\n"
              | '\012' -> "\\f"
              | '\r' -> "\\r"
              | '\t' -> "\\t"
              | ch ->
                 if (ch < ' ')
                 then Printf.sprintf "\\x%x;" (int_of_char ch)
                 else Printf.sprintf "%c" ch)
             (string_to_list str)))
  | ScmSymbol(sym) -> sym
  | ScmNumber(ScmRational(0, _)) -> "0"
  | ScmNumber(ScmRational(num, 1)) -> Printf.sprintf "%d" num
  | ScmNumber(ScmRational(num, -1)) -> Printf.sprintf "%d" (- num)
  | ScmNumber(ScmRational(num, den)) -> Printf.sprintf "%d/%d" num den
  | ScmNumber(ScmReal(x)) -> Printf.sprintf "%f" x
  | ScmVector(sexprs) ->
     let strings = List.map string_of_sexpr sexprs in
     let inner_string = String.concat " " strings in
     Printf.sprintf "#(%s)" inner_string
  | ScmPair(ScmSymbol "quote",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf "'%s" (string_of_sexpr sexpr)
  | ScmPair(ScmSymbol "quasiquote",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf "`%s" (string_of_sexpr sexpr)
  | ScmPair(ScmSymbol "unquote",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf ",%s" (string_of_sexpr sexpr)
  | ScmPair(ScmSymbol "unquote-splicing",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf ",@%s" (string_of_sexpr sexpr)
  | ScmPair(car, cdr) ->
     string_of_sexpr' (string_of_sexpr car) cdr
and string_of_sexpr' car_string = function
  | ScmNil -> Printf.sprintf "(%s)" car_string
  | ScmPair(cadr, cddr) ->
     let new_car_string =
       Printf.sprintf "%s %s" car_string (string_of_sexpr cadr) in
     string_of_sexpr' new_car_string cddr
  | cdr ->
     let cdr_string = (string_of_sexpr cdr) in
     Printf.sprintf "(%s . %s)" car_string cdr_string;;