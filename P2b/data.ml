open Funs

(***********************)
(* Part 2: Integer BST *)
(***********************)

type int_tree =
    IntLeaf
  | IntNode of int * int_tree * int_tree

let empty_int_tree = IntLeaf

let rec int_insert x t =
    match t with
      IntLeaf -> IntNode(x,IntLeaf,IntLeaf)
    | IntNode (y,l,r) when (x > y) -> IntNode (y,l,int_insert x r)
    | IntNode (y,l,r) when (x = y) -> t
    | IntNode (y,l,r) -> IntNode(y,int_insert x l,r)

let rec int_mem x t =
    match t with
      IntLeaf -> false
    | IntNode (y,l,r) when (x > y)-> int_mem x r
    | IntNode (y,l,r) when (x = y) -> true
    | IntNode (y,l,r) -> int_mem x l

(* Implement the functions below. *)

let rec int_size t = 
  match t with
  IntLeaf -> 0
  | IntNode(y,l,r) -> (int_size l + int_size r + 1)
;;

let rec int_min t = match t with
IntLeaf -> raise (Invalid_argument("int_min"))
|IntNode(y,l,r) when l = IntLeaf -> y
|IntNode(y,l,r) -> int_min l
;;

let rec int_insert_all lst t = 
  fold (fun a b -> int_insert b a) t lst
;;

let rec int_as_list t = 
  match t with
  IntLeaf -> []
  | IntNode(y,l,r) -> (int_as_list l)@[y]@(int_as_list r) 
  
;;
let rec int_common t x y = 
  if (not(int_mem x t) || not(int_mem y t))
    then raise(Invalid_argument("int_common"))
  else match t with
  IntLeaf -> raise (Invalid_argument("int_common"))
  | IntNode(h,l,r) ->
    if (x > h) && (y > h) then int_common r x y
    else if (x < h) && (y < h) then int_common l x y
    else h
;;

(***************************)
(* Part 3: Polymorphic BST *)
(***************************)

type 'a atree =
    Leaf
  | Node of 'a * 'a atree * 'a atree
type 'a compfn = 'a -> 'a -> int
type 'a ptree = 'a compfn * 'a atree

let empty_ptree f : 'a ptree = (f,Leaf)

(* Implement the functions below. *)
let rec pinsert_helper x (a,b) =
  match b with
    Leaf -> (a,Node(x,Leaf,Leaf))
    | Node (y,l,r) ->
      if  (a x y) < 0 then
        let (_,v) = pinsert_helper x (a,l) in (a,Node(y,v,r))
      else if (a x y) > 0 then
        let (_,k) = pinsert_helper x (a,r) in (a,Node(y,l,k))
      else (a,b)
;;
let pinsert x t = pinsert_helper x t
;;


let rec pmem_helper x (b,t) = match t with
  Leaf -> false
  | Node (y,l,r) ->
    if (b x y ) = 0 then true
    else if (b x y) > 0 then pmem_helper x (b,r) 
    else pmem_helper x (b,l);;

let pmem x t = pmem_helper x t
;;

(*******************************)
(* Part 4: Graphs with Records *)
(*******************************)

type node = int
type edge = { src : node; dst : node; }
type graph = { nodes : int_tree; edges : edge list; }

let empty_graph = {nodes = empty_int_tree; edges = [] }

let add_edge e { nodes = ns; edges = es } =
    let { src = s; dst = d } = e in
    let ns' = int_insert s ns in
    let ns'' = int_insert d ns' in
    let es' = e::es in
    { nodes = ns''; edges = es' }

let add_edges es g = fold (fun g e -> add_edge e g) g es

(* Implement the functions below. *)

let graph_empty g = 
  if g = empty_graph then true else false
;;

let graph_size g = 
  int_size g.nodes
;;

let is_dst n e = 
  if e.dst = n then true 
  else false
;;


let src_edges n g = 
  if graph_empty g = true then []
  else
  fold (fun a b -> if b.src = n then b::a else a) [] g.edges
;;


let append x y =
  fold_right (fun z a -> z::a) x y;;
  
  let reachable n g =
    if int_mem n g.nodes = false then empty_int_tree
    else let rec helper t h g =
    match h with
    | [] -> t
    | a::b -> if int_mem a t = false then
    helper (int_insert a t) (append b (map (fun e -> e.dst) (src_edges a g))) g else
    helper t b g
    in  helper empty_int_tree [n] g 
  ;;