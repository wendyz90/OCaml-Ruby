open Sets
open Nfa

(*********)
(* Types *)
(*********)

type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t

(*******************************)
(* Part 2: Regular Expressions *)
(*******************************)

let rec change_new_qs l n =
	match l with
	  [] -> []
	| h :: t -> ((h + n) :: (change_new_qs t n))
;;

let rec change_new_deltas l n =
	match l with
	  [] -> []
	| (s, f, e) :: t -> (((s + n), f, (e + n)) :: (change_new_deltas t n))
;;

let rec remove l x =
	match l with
	  [] -> []
	| h :: t -> 
		begin
			if (x = h) then (remove t x)
			else (h :: (remove t x))
		end
;;

let rec del_dups l1 l2 =
	match l1 with
	  [] -> l2
	| h :: t ->
		begin
			let new_l2 = (remove l2 h) in
			del_dups t new_l2
		end
;;

let get_last end_s =
	match end_s with
	  [] -> -1
	| h :: _ -> h
;;
		

let rec regexp_to_nfa re =
	match re with
	  Empty_String ->
		begin
			{
				qs = [0;1];
				sigma = [];
				delta = [(0, None, 1)];
				q0 = 0;
				fs = [1];
			}
		end
	| Char c ->
			{
				qs = [0; 1];
				sigma = [c];
				delta = [(0, (Some c), 1)];
				q0 = 0;
				fs = [1];
			}
	| Union (exp1, exp2) ->
			let nfa1 = regexp_to_nfa exp1 in
			let nfa2 = regexp_to_nfa exp2 in
			
			let move = (2 + (List.length nfa1.qs)) in
			
			let nfa1_qs = (change_new_qs nfa1.qs 2) in
			let nfa2_qs = (change_new_qs nfa2.qs move) in
			
			let end_nfa1 = (2 + (get_last nfa1.fs)) in
			let end_nfa2 = (move + (get_last nfa2.fs)) in
			
			let nfa1_deltas = (change_new_deltas nfa1.delta 2) in
			let nfa2_deltas = (change_new_deltas nfa2.delta move) in
			
			{
				qs = (0 :: 1 :: (List.append nfa1_qs nfa2_qs));
				sigma = (List.append nfa1.sigma (del_dups nfa1.sigma nfa2.sigma));
				delta = (
				(0, None, 2) ::
				(0, None, move) ::
				(end_nfa1, None, 1) ::
				(end_nfa2, None, 1) ::
				(List.append nfa1_deltas nfa2_deltas));
				q0 = 0;
				fs = [1];
			}
	| Concat (exp1, exp2) ->
			let nfa1 = regexp_to_nfa exp1 in
			let nfa2 = regexp_to_nfa exp2 in
			
			let move = (2 + (List.length nfa1.qs)) in
			
			let qs1 = (change_new_qs nfa1.qs 2) in
			let qs2 = (change_new_qs nfa2.qs move) in
			
			let f_nfa1 = (2 + (get_last nfa1.fs)) in
			let f_nfa2 = (move + (get_last nfa2.fs)) in
			
			let delta1 = (change_new_deltas nfa1.delta 2) in
			let delta2 = (change_new_deltas nfa2.delta move) in
			
			{
				qs = (0 :: 1 :: (List.append qs1 qs2));
				sigma = (List.append nfa1.sigma (del_dups nfa1.sigma nfa2.sigma));
				delta = (
				(0, None, 2) ::
				(f_nfa1, None, move) ::
				(f_nfa2, None, 1) ::
				(List.append delta1 delta2));
				q0 = 0;
				fs = [1];
			}
	| Star exp ->
			let nfa = regexp_to_nfa exp in
			let qs = (change_new_qs nfa.qs 2) in
			let f_nfa = (2 + (get_last nfa.fs)) in
			
			let deltas = (change_new_deltas nfa.delta 2) in
			{
				qs = (0 :: 1 :: qs);
				sigma = (nfa.sigma);
				delta = (
				(0, None, 1) ::
				(0, None, 2) ::
				(1, None, 0) ::
				(f_nfa, None, 1) ::
				deltas);
				q0 = 0;
				fs = [1];
			}
;;

let rec regexp_to_string r =
	match r with
	  Empty_String -> "E"
	| Char c -> (String.make 1 c)
	| Union (a, b) -> "(" ^ (regexp_to_string a) ^ "|" ^ (regexp_to_string b) ^ ")"
	| Concat (a, b) -> "(" ^(regexp_to_string a) ^ (regexp_to_string b) ^ ")"
	| Star exp -> "(" ^ (regexp_to_string exp) ^ ")*"
;;

(*****************************************************************)
(* Below this point is parser code that YOU DO NOT NEED TO TOUCH *)
(*****************************************************************)

exception IllegalExpression of string

(* Scanner *)
type token =
  | Tok_Char of char
  | Tok_Epsilon
  | Tok_Union
  | Tok_Star
  | Tok_LParen
  | Tok_RParen
  | Tok_END

let tokenize str =
  let re_var = Str.regexp "[a-z]" in
  let re_epsilon = Str.regexp "E" in
  let re_union = Str.regexp "|" in
  let re_star = Str.regexp "*" in
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let rec tok pos s =
    if pos >= String.length s then
      [Tok_END]
    else begin
      if (Str.string_match re_var s pos) then
        let token = Str.matched_string s in
        (Tok_Char token.[0])::(tok (pos+1) s)
      else if (Str.string_match re_epsilon s pos) then
        Tok_Epsilon::(tok (pos+1) s)
      else if (Str.string_match re_union s pos) then
        Tok_Union::(tok (pos+1) s)
      else if (Str.string_match re_star s pos) then
        Tok_Star::(tok (pos+1) s)
      else if (Str.string_match re_lparen s pos) then
        Tok_LParen::(tok (pos+1) s)
      else if (Str.string_match re_rparen s pos) then
        Tok_RParen::(tok (pos+1) s)
      else
        raise (IllegalExpression("tokenize: " ^ s))
    end
  in
  tok 0 str

let tok_to_str t = ( match t with
      Tok_Char v -> (Char.escaped v)
    | Tok_Epsilon -> "E"
    | Tok_Union -> "|"
    | Tok_Star ->  "*"
    | Tok_LParen -> "("
    | Tok_RParen -> ")"
    | Tok_END -> "END"
  )

(*
   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen
   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let parse_regexp (l : token list) =
  let lookahead tok_list = match tok_list with
      [] -> raise (IllegalExpression "lookahead")
    | (h::t) -> (h,t)
  in

  let rec parse_S l =
    let (a1,l1) = parse_A l in
    let (t,n) = lookahead l1 in
    match t with
      Tok_Union -> (
        let (a2,l2) = (parse_S n) in
        (Union (a1,a2),l2)
      )
    | _ -> (a1,l1)

  and parse_A l =
    let (a1,l1) = parse_B l in
    let (t,n) = lookahead l1 in
    match t with
      Tok_Char c ->
      let (a2,l2) = (parse_A l1) in (Concat (a1,a2),l2)
    | Tok_Epsilon ->
      let (a2,l2) = (parse_A l1) in (Concat (a1,a2),l2)
    | Tok_LParen ->
      let (a2,l2) = (parse_A l1) in (Concat (a1,a2),l2)
    | _ -> (a1,l1)

  and parse_B l =
    let (a1,l1) = parse_C l in
    let (t,n) = lookahead l1 in
    match t with
      Tok_Star -> (Star a1,n)
    | _ -> (a1,l1)

  and parse_C l =
    let (t,n) = lookahead l in
    match t with
      Tok_Char c -> (Char c, n)
    | Tok_Epsilon -> (Empty_String, n)
    | Tok_LParen ->
      let (a1,l1) = parse_S n in
      let (t2,n2) = lookahead l1 in
      if (t2 = Tok_RParen) then
        (a1,n2)
      else
        raise (IllegalExpression "parse_C 1")
    | _ -> raise (IllegalExpression "parse_C 2")
  in
  let (rxp, toks) = parse_S l in
  match toks with
  | [Tok_END] -> rxp
  | _ -> raise (IllegalExpression "parse didn't consume all tokens")

let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str