open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q
type ('q, 's) nfa_t = {
  qs : 'q list;
  sigma : 's list;
  delta : ('q, 's) transition list;
  q0 : 'q;
  fs : 'q list;
}

(*********************)
(* Utility Functions *)
(*********************)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec fix comp f x0 =
  let next = f x0 in
  if comp x0 next then x0 else fix comp f next

(****************)
(* Part 1: NFAs *)
(****************)

let rec move_helper  del a b  = 
  match del with 
  |[]->[]
  | f :: s -> match f with
  | (x, sigm, y)-> if  x = a && sigm = b then union [y] (move_helper s a b ) else (move_helper s a b )
  ;;

let rec move m qs s = 
  match qs with 
  | []->[]
  | h::t -> union (move m t s) (move_helper (m.delta) h s)
  ;;

		
let e_closure m qs = let f x = union (move m x None ) x  in 
fix eq f qs
	
;;
let include_final finals states = 
  if intersection finals states == [] then false else true
  ;;
let rec get_lst_st m chr sts  = 
    match chr with
	| [] -> e_closure m sts
	| e :: f -> get_lst_st m f (move m (e_closure m sts) (Some e))
;;
let accept m str = 
  include_final (m.fs) 
  (get_lst_st m (explode str) (e_closure m (m.q0 :: [])))
    ;;


let helper f1 f2 = 
  {
    qs = union f1.qs f2.qs; 
    sigma = union f1.sigma f2.sigma;
    delta = union f1.delta f2.delta;
    q0 = union f1.q0 f2.q0;
    fs = union f1.fs f2.fs;
  }
let nfa_to_dfa m = 
  let a = e_closure m [m.q0] in 
  let final_st = intersection m.fs a in 
  let b = {
    qs = [a];
    sigma = m.sigma;
    delta = [];
    q0 = a;
    fs = if eq final_st [] then []
    else [a];
  }in
  let update nfa = let rec repeat qs =
  match qs with 
  |[] -> nfa
  | h :: t -> let rec iterA sigm = match sigm with 
      |[]-> nfa
      | h1::t1 -> let s = e_closure m (move m h (Some h1))in
		let final_s = intersection m.fs s in 

		let c = helper nfa {
        qs = if eq s [] then [] else [s];
        sigma = [];
        delta = if eq s [] then [] else [(h, (Some h1), s)];
        q0 = [];
        fs = if eq final_s [] then []
        else [s];
      }in helper c (iterA t1) in
      helper (iterA (m.sigma)) (repeat t) in 
      repeat nfa.qs
      in let compare f1 f2 = if (eq f1.qs f2.qs) && (eq f1.sigma f2.sigma)&& (eq f1.delta f2.delta)&& (eq f1.q0 f2.q0)&& (eq f1.fs f2.fs)
      then true else false
      in
	fix compare update b 
;;
