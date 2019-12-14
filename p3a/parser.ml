

type stmt_result = token list * stmt
type expr_result = token list * expr

(* open SmallCTypes
open Utils Provided helper function - takes a token list and an exprected token.
 * Handles error cases and returns the tail of the list *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))


let lookahead toks  =
  match toks with
  [] -> raise (InvalidInputException ("empty"))
  | (h::t) -> h  


let rec parse_expr toks =
    if toks <> [EOF]
    then
       parse_Or toks
    else raise (InvalidInputException(""))



and parse_Or toks =
    let l,e = parse_Ad toks in
    match (lookahead l) with
    |Tok_Or ->
        let l1 = match_token l (Tok_Or) in
        let l2,e1 = parse_Or l1 in
        l2,Or(e, e1)
    |_ -> l,e

and parse_Ad toks =
    let l,e = parse_Eq toks in
    match (lookahead l) with
    |Tok_And ->
        let l1 = match_token l (Tok_And) in
        let l2,e1 = parse_Ad l1 in
        l2,And(e, e1)
    |_ -> (l,e )   

and parse_Eq toks =
    let l,e = parse_Re toks in

    if (lookahead l) = Tok_Equal || (lookahead l) = Tok_NotEqual then
			let l2 = match_token t1 (lookahead l) in
			let (l,e2) = parse_Eq l2 in
			match (lookahead l) with
				Tok_Equal -> (t,Equal(e,e1))
			|	Tok_NotEqual -> (t,NotEqual(e,e1))
			|	_ -> raise (InvalidInputException("parse_Eq"))
	else
		(l,e)

and parse_Re toks =
    let l,e = parse_Add toks in
    if (lookahead l) = Tok_Greater || (lookahead l) = Tok_Less || (lookahead l) = Tok_GreaterEqual || (lookahead l) = Tok_LessEqual then
        match (lookahead l) with
            |Tok_Greater ->
                let l1 = match_token l (Tok_Greater) in
                let l2,e1 = parse_Re l1 in 
                l2,Greater(e, e1)
    
            |Tok_Less ->
                let l1 = match_token l (Tok_Less) in
                let l2,e1 = parse_Re l1 in 
                l2,Less(e, e1)

            |Tok_GreaterEqual ->
                let l1 = match_token l (Tok_GreaterEqual) in
                let l2,e1 = parse_Re l1 in
                l2,GreaterEqual(e, e1)
        
            |Tok_LessEqual ->
                let l1 = match_token l (Tok_LessEqual) in
                let l2,e1 = parse_Re l1 in 
                l2,LessEqual(e, e1)
            | _ -> raise (InvalidInputException("parse_Re"))
    else (l,e)
    
and parse_Add toks =
    let l,e = parse_Mu toks in
    if (lookahead l) = Tok_Plus || (lookahead l) = Tok_Sub then
        match (lookahead l)with
            |Tok_Add ->
                let l1 = match_token l (Tok_Add) in
                let l2,e1 = parse_Add l1 in
                l2,Add(e, e1)

            |Tok_Sub ->
                let l1 = match_token l (Tok_Sub) in
                let  l2,e1  = parse_Add l1 in 
                l2,Sub(e, e1)

            | _ -> raise (InvalidInputException("parse_Add"))
    else  (l,e)

and parse_Mu toks =
    let l,e = parse_Pow toks in
    if (lookahead l) = Tok_Mult || (lookahead l) = Tok_Div then
        match (lookahead l) with
            |Tok_Mult ->
                let l1 = match_token l (Tok_Mult) in
                let l2,e1 =  parse_Mu l1 in
                l2,Mult(e, e1)
            |Tok_Div ->
                let l1 = match_token l (Tok_Div) in
                let l2,e1  = parse_Mu l1 in
                l2,Div(e,e1)
            |	_ -> raise (InvalidInputException("parse_Mu"))
    |_ -> (l,e)

and parse_Pow toks =
    let (l, e) = parse_Unary toks in
    match (lookahead l) with
    |Tok_Pow ->  
        let l1 = match_token l (Tok_Pow) in
        let (l2,e1) = parse_Pow l1 in
        l2,Pow(e,e1)
    |_ -> (l,e)

and parse_Unary toks =
    match (lookahead toks) with
    |Tok_Not ->
        let l = match_token toks  (Tok_Not) in
        let (l1, e) = parse_Unary l in
        l1,Not(e)
    |_ -> parse_Pri toks

and parse_Pri toks =
    match (lookahead toks) with
    |Tok_Int a ->
        let l = match_token toks  (Tok_Int a) in
        (l,Int(a))
    |Tok_Bool a ->
        let  l = match_token toks (Tok_Bool a)in
        l,Bool(a)
    |Tok_ID a ->
        let l = match_token toks  (Tok_ID a) in    
        l,ID(a)    
    |Tok_LParen ->
        let l = match_token toks (Tok_LParen) in
        let l1,e = parse_expr l in
        let l2 = match_token l1 (Tok_RParen)in
        l2,e
    |    _ -> raise (InvalidInputException("parse_Pri"))    

 


let rec parse_stmt toks =
 let (l1,s) = parse_St toks in
        if s = NoOp then
          (l1,s)
     
        else
        let(l2,s1) = parse_stmt l1 in
        (l2,Seq(s,s1))

and parse_St toks =

    match lookahead toks  with
    
    |(Tok_Int_Type ) -> parse_Int toks
    |(Tok_Bool_Type) -> parse_Bool toks
    |Tok_If -> parse_If toks
    |Tok_While -> parse_Whi toks
    |Tok_ID (s) -> parse_Id toks
    |Tok_Print -> parse_Print toks
    
    | _ -> (toks,NoOp)
    
and match_V toks = 
let lst = match_token toks (lookahead toks )in
    (lst,(lookahead toks ))

and parse_Int toks=
    let (l,t_type) = match_V toks in
    let (l1,t_id) = match_V l in
    let (l2,t_semi) = match_V l1 in
        match (t_type,t_id,t_semi) with
            (Tok_Int_Type,Tok_ID(s),Tok_Semi) -> (t,Declare(Type_Int,s))
            |_ ->  raise (InvalidInputException(" parse_Int"))


and parse_Bool toks=
    let (l,t_type) = match_V toks in
    let (l1,t_id) = match_V l in
    let (l2,t_semi) = match_V l1 in
        match (t_type,t_id,t_semi) with
            (Tok_Bool_Type,Tok_ID(s),Tok_Semi) -> (t,Declare(Type_Int,s))
            |_ ->  raise (InvalidInputException(" parse_Bool"))



and parse_Whi toks =
    let l = match_token toks (Tok_While) in
    let l1,e = parse_expr l in
    let l2 = match_token l1 (Tok_LBrace) in
    let l3,s1 = parse_stmt l2 in
    let l4 = match_token l3 (Tok_RBrace) in
    l4,While(e,s1)

and parse_If toks =
    let l = match_token toks (Tok_If) in
    let l1,e = parse_expr l in
    let l2 = match_token l1 (Tok_LBrace) in
    let l3,s1 = parse_stmt l2 in
    let l4 = match_token l3 (Tok_RBrace) in
    let l5,s2 = parse_Else l4 in
     l5,If(e,s1,s2) 
   

and parse_Else toks =
    if (lookahead toks) = Tok_Else then
    let l5 = match_token toks (Tok_Else) in
    let l6 = match_token l5 (Tok_LBrace) in
    let l7,s = parse_stmt l6 in
    let l8 = match_token l7 (Tok_RBrace) in
    l8,s
    else
        toks, NoOp

and parse_Id toks =
    let (l, id) = match_V toks  in
    let (l1,t) = match_V l  in
    let (l2,t1) = parse_expr l1 in
    let (l3,semi)= match_V l2 in

    match (id,t,semi) with
        (Tok_ID (s),Tok_Assign,Tok_Semi) -> (l3,Assign(s,t1))        

        |_ -> raise (InvalidInputException("parse_Id"))
      
and parse_Print toks =

    let (l, a) = match_V toks  in
    let (l1,b) = match_V l  in
    let (l2,c) = parse_expr l1 in
    let (l3,d) = match_V l2
    let (l4,semi)= match_V l3 in
        match (a,b,d,semi) with
            (Tok_Print,Tok_LParen,Tok_RParen,Tok_Semi) -> (t,Print(c))
        |_ raise (InvalidInputException("parse_Print"))



let parse_main toks =
    match toks with
    |Tok_Int_Type::(Tok_Main::(Tok_LParen::(Tok_RParen::(Tok_LBrace::t))))
    ->
        let l,s = parse_stmt t in
        if (match_token l (Tok_RBrace)) = [EOF] then  s
        else
        raise (InvalidInputException (string_of_list string_of_token l))
    |_ ->  raise (InvalidInputException ("N/A "))
    
;;

    