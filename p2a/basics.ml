(******************************)
(* Part 1: Non-List Functions *)
(******************************)

let divides x y = 
  if x = 0 then false
  else if y = 0 then true 	
	else if y mod x =0 then true
	else false
;;

let rec gcd a b = 
  if b = 0 then a else gcd b (a mod b)
;;


let rec ack m n = 
  if m = 0 then (n + 1) 
  else if n = 0 then (ack (m-1) 1) 
  else (ack( m -1) (ack m (n-1)))
;;

(*********************************)
(* Part 2: Simple List Functions *)
(*********************************)

let second_element lst =  
  match lst with
  [] -> -1
  | (h::[]) -> -1
  | (h::t::_) ->t
;;

let max_first_three lst = 
  match lst with
  [] -> 0
  | (h::[]) -> h
  | (h::t::[]) -> h+t
  | (h::t::z::_) -> h+t+z
;;

let max_first_three lst = 
  match lst with
    [] -> 0
  | e1::[]-> e1 
  | e1::e2::[] -> if e1 > e2 then e1 else e2
  | e1::e2::e3::t -> if (e1 >= e2) && (e1 >= e3) then e1
                     else if (e1 >= e2) && (e1 <= e3) then e3
                     else e3  
;;

(************************************)
(* Part 3: Recursive List Functions *)
(************************************)

let rec partial_sum i lst = 
  match lst with
  [] -> (-1)
  | (h::t) -> if i<0 then (-1)
       else if i=0 then h
       else partial_sum (i-1) t
;;


let rec partial_sums is lst = 
  match is with
  [] -> []
  | (h::t) -> (partial_sum h lst)::(partial_sums t lst)
;;

let rec zip lst1 lst2 = 
  match lst1,lst2 with
  [],_ -> []
  |_, [] ->[]
  |(a::t1),(b::t2) -> (a,b)::(zip t1 t2)
  
;;

let rec index_help x lst curr = 
  match lst with
  [] -> -1
  |a::b -> if a = x then curr else (index_help x b (curr+1))
;;

let rec index x lst =  (index_help x lst 0)
;;

(****************)
(* Part 4: Sets *)
(****************)

let rec elem x a =  
  match a with
  [] -> false
  | (h::t) -> if x=h then true
       else (elem x t)
;;

let rec insert x a = 
  match a with
  [] -> [x]
  | h::t -> if x < h then x::h::t 
            else if x = h then x::t 
            else h::(insert x t)
;;

let rec subset a b = 
  match a with
  [] -> true
  | (h::t) -> if (index h b)=(-1) then false
              else (subset t b)
;;	

let rec eq a b =  (subset a b)&&(subset b a)
;;

let rec remove x a =  
  match a with
  [] -> []
  | (h::t) -> if h=x then remove x t
      else h::(remove x t)
;;

let rec union a b =  
  match a with
  [] -> b
  | (h::t) -> union t (insert h b)
;;

let rec intersection a b =  
  match a with
  [] -> [] 
  | (h::t) -> if (elem h b) then h::(intersection t b)
       else intersection t b
;;

let rec product_help x b = 
  match x with 
  |[]-> b
  | h::t -> h::(t @ b)
;;
  
let rec product a b  = 
  let rec helpr x lst = match lst with
  [] -> []
  | (h1 :: t1) -> (x, h1) :: helpr x t1
  in match a with
  [] -> []
  | (h2 :: t2) -> product_help (helpr h2 b) (product t2 b)
  ;;
 
