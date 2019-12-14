open Types
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError


let rec lookup env x = 
	match env with
	  [] -> raise (DeclareError "")
    | (y,v)::env_t -> 
    if x = y then v
    else lookup env_t x
;;

let rec eval_expr env e = 
  match e with
  | Int x -> Int_Val(x)

  | Bool x -> Bool_Val(x)

  | ID x -> lookup env x

  | Add(x,y)-> 
  (
    let ele1 = eval_expr env x in
      let ele2 = eval_expr env y in
        match ele1, ele2 with 
          Int_Val a, Int_Val b ->  Int_Val(a+b) 
		      | _ ->  raise (TypeError "Add Type Error")
  )

  | Sub (x,y)->
  (
    let ele1 = eval_expr env x in
      let ele2 = eval_expr env y in
      match ele1, ele2 with 
        Int_Val a, Int_Val b -> Int_Val(a-b)
        | _ -> raise (TypeError "Sub Type Error")
  )

  | Mult (x,y) ->
  (
    let ele1 = eval_expr env x in
      let ele2 = eval_expr env y in
      match ele1, ele2 with 
        Int_Val a , Int_Val b -> Int_Val(a*b)
        | _ ->raise (TypeError "Mult Type Error")
  )

  | Div (x,y)->
  (
    let ele1 = eval_expr env x in
      let ele2 = eval_expr env y in
      match ele1, ele2 with 
        Int_Val a , Int_Val b -> if b = 0 then raise DivByZeroError
          else Int_Val(a/b) 
        | _ -> raise (TypeError "Div Type Error")
  )
  
  | Pow (x, y)->
  (
    let ele1 = eval_expr env x in
      let ele2 = eval_expr env y in
      match ele1, ele2 with 
        Int_Val a , Int_Val b -> Int_Val(int_of_float(float_of_int(a) ** float_of_int(b)))
       | _ -> raise (TypeError "Pow Type Error")
  )

  | Or (x,y) -> 
	(
		 let ele1 = eval_expr env x in
		 let ele2 = eval_expr env y in 
		 match ele1,ele2 with
		 Bool_Val a, Bool_Val b -> Bool_Val(a||b)
		 | _ -> raise (TypeError "Or Type Error")
	 )

	 
	| And (x,y) ->
	(
		 let ele1 = eval_expr env x in
		 let ele2 = eval_expr env y in 
		 match ele1,ele2 with
		 Bool_Val a, Bool_Val b -> Bool_Val(a&&b)
		 | _ -> raise (TypeError "And Type Error")
	 )
	 
	 
	| Not (x) ->
	(
		 let ele1 = eval_expr env x in
		 match ele1 with
		 Bool_Val a -> Bool_Val(not a)
		 | _ -> raise (TypeError "Not Type Error")
	 )



   | Greater (x,y) ->
   (
      let ele1 = eval_expr env x in
      let ele2 = eval_expr env y in
      match ele1, ele2 with
      Int_Val a, Int_Val b -> if a > b then Bool_Val(true) else Bool_Val(false)
      | _ -> raise (TypeError "Greater Type Error")
      
    )
 
    
    
   | Less (x,y) ->
   (
      let ele1 = eval_expr env x in
      let ele2 = eval_expr env y in
      match ele1, ele2 with
      Int_Val a, Int_Val b -> if a < b then Bool_Val(true) else Bool_Val(false)
      | _ -> raise (TypeError "Less Type Error")
    )
    
    
    
   | GreaterEqual (x,y) -> 
   (
      let ele1 = eval_expr env x in
      let ele2 = eval_expr env y in
      match ele1, ele2 with
      Int_Val a, Int_Val b -> if a >= b then Bool_Val(true) else Bool_Val(false)
      | _ -> raise (TypeError "GreaterEqual Type Error")
    )
    
    
    
   | LessEqual (x,y) -> 
   (
      let ele1 = eval_expr env x in
      let ele2 = eval_expr env y in
      match ele1, ele2 with
      Int_Val a, Int_Val b -> if a <= b then Bool_Val(true) else Bool_Val(false)
      | _ -> raise (TypeError "LessEqual Type Error")
    )
    
    
   | Equal (x,y) -> 
   (
      let ele1 = eval_expr env x in
      let ele2 = eval_expr env y in
      match ele1, ele2 with
      (Int_Val a), (Int_Val b) -> if a = b then Bool_Val(true) else Bool_Val(false)
     | (Bool_Val a), (Bool_Val b) -> if a = b then Bool_Val(true) else Bool_Val(false)
     | _ -> raise (TypeError "Equal Type Error")
    )
    
   | NotEqual (x,y) -> 
   (
      let ele1 = eval_expr env x in
      let ele2 = eval_expr env y in
      match ele1, ele2 with
      (Int_Val a), (Int_Val b) -> if a <> b then Bool_Val(true) else Bool_Val(false)
     | (Bool_Val a), (Bool_Val b) -> if a <> b then Bool_Val(true) else Bool_Val(false)
     | _ -> raise (TypeError "NotEqual Type Error")
    )
    

  ;;







let rec finder env n = match env with
	[] -> false
	| (x,y)::t -> if x = n then true
					else finder t n
;;



let rec change str env v = match env with
	[] -> []
	| (x,y)::t -> if str = x then (x,v)::t 
					else (x,y)::(change str t v)
;;


let rec eval_stmt env s = match s with

NoOp -> env
	 
| Seq (a,b) -> eval_stmt (eval_stmt env a) b; 


| Declare (i, str) -> 

  if finder env str then raise (DeclareError "declare Declaration Error")
      
  else 
  (
    match i with 
    Bool_Type -> ((str,Bool_Val(false))::env)
	| Int_Type -> ((str,Int_Val(0))::env)
  ) 


| Assign (s,e) -> 

  if (finder env s) = false then  raise (DeclareError "Assign Declaration Error")
  else 
  (
    let x = eval_expr env e in match x with 
      Int_Val i ->
      (
        let y = lookup env s in match y with 
        Int_Val i2 -> change s env x
        | _ -> raise (TypeError "Assign Type Error")
      )
        
        
      | Bool_Val b -> 
      (
        let z = lookup env s in match z with 
        Bool_Val i3 -> change s env x
        | _ -> raise (TypeError "Assign Type Error")
      )		
  )



| If (e,s1,s2) -> 
(
  let v = eval_expr env e in match v with 
  Bool_Val b -> if b then eval_stmt env s1 
          else eval_stmt env s2
  | _ -> raise (TypeError "If typer Error")
)
  
| While (e,s) -> 
 (
  let x = eval_expr env e in match x with
  Bool_Val(true) -> eval_stmt (eval_stmt env s) (While(e,s))
  | Bool_Val(false) -> env
  
  | _ -> raise  (TypeError "While Type Error")
 
 )
 


| Print e -> 
(let x = eval_expr env e in match x with
  | Int_Val a -> print_output_int a; print_output_newline(); env

  | Bool_Val b -> print_output_bool b; print_output_newline(); env
)

;;
