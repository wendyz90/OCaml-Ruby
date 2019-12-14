open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

(* Returns how many times the element e occurs in lst *)
let count e lst =
  fold (fun a h -> if h = e then a + 1 else a) 0 lst
  ;;

(* Returns a list of booleans where each boolean represents
  if the corresponding element of lst is divisible by n. Take note that 0 is
  divisible by 0. *)
let divisible_by n lst =
  if n != 0 then
    map (fun a -> if a mod n = 0 then true else false) lst
  else
    map (fun a -> if a = 0 then true else false) lst
  ;;


(* Returns a list of booleans where each boolean represents if
  the corresponding element of lst is divisible by the first element of lst.
  Take note that 0 is divisible by 0 *)
let divisible_by_first lst =
  match lst with
  [] -> []
  |h::t -> divisible_by h lst
  ;;


(* Returns a list of tuples containing every possible combination of
  elements from lst1 and lst2 and nothing more. *)
let pairup lst1 lst2 =
  fold (fun a h -> append a (map (fun b -> (h,b)) lst2)) [] lst1;
  ;;

(* Returns a list consisting of the elements of lst concatenated together.
  Note that only the top level of lists is concatenated,
  whereas List.flatten concatenates all levels *)
let concat_lists lst =
  fold (fun a h -> append a h) [] lst
  ;;

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
    | IntNode (y,l,r) when x > y -> IntNode (y,l,int_insert x r)
    | IntNode (y,l,r) when x = y -> t
    | IntNode (y,l,r) -> IntNode(y,int_insert x l,r)

let rec int_mem x t =
    match t with
      IntLeaf -> false
    | IntNode (y,l,r) when x > y -> int_mem x r
    | IntNode (y,l,r) when x = y -> true
    | IntNode (y,l,r) -> int_mem x l

(* Implement the functions below. *)

(* Returns the number of nodes in tree t *)
let rec int_size t =
  match t with
    IntLeaf -> 0
    |IntNode(y,l,r) -> 1 + int_size l + int_size r
  ;;

(* Returns the maximum element in tree t. Raises exception
  Invalid_argument("int_max") on an empty tree. This function should be O
  (height of the tree). *)
let rec int_max t =
  match t with
  IntLeaf -> raise (Invalid_argument("int_max"))
  |IntNode(y,l,r) when r = IntLeaf -> y
  |IntNode(y,l,r) -> int_max r
  ;;


(* Returns a tree which is the same as tree t, but with all the integers in
  list lst added to it. Try to use fold to implement this in one line. *)
let rec int_insert_all lst t =
  fold (fun a h -> int_insert h a) t lst
  ;;

(* Returns a list where the values correspond to an in-order traversal
  on tree t *)
let rec int_as_list t =
  match t with
  IntLeaf -> []
  |IntNode(y,l,r) -> append(int_as_list l)(y::int_as_list r)
  ;;


(* Returns the closest common ancestor of x and y in the tree t
  (i.e. the lowest shared parent in the tree). Raises exception Invalid_argument
  ("int_common") on an empty tree or where x or y don't exist in tree t *)
let rec int_common t x y =
  if (not(int_mem x t) || not(int_mem y t))
    then raise(Invalid_argument("int_common"))
  else match t with
    IntLeaf -> raise (Invalid_argument("int_common"))
    |IntNode(h,l,r) ->
      if x > h && y > h then int_common r x y
      else if x < h && y < h then int_common l x y
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

(* Returns a tree which is the same as tree t, but with x added to it *)
let rec pinsert_helper x (b,t) =
  match t with
    Leaf -> (b,Node(x,Leaf,Leaf))
    |Node (y,l,r) ->
      if  (b x y) < 0 then
        let (_,v) = pinsert_helper x (b,l) in (b,Node(y,v,r))
      else if (b x y) > 0 then
        let (_,k) = pinsert_helper x (b,r) in (b,Node(y,l,k))
      else (b,t)
;;

let pinsert x t = pinsert_helper x t
;;

(* Returns true iff x is an element of tree t *)
let rec pmem_helper x (b,t) = match t with
  Leaf -> false
  |Node (y,l,r) ->
    if (b x y) < 0 then pmem_helper x (b,l)
    else if (b x y) > 0 then pmem_helper x (b,r)
    else true
;;

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


(* Returns true iff graph g is empty. *)
let graph_empty g =
  if g = empty_graph then true else false
  ;;

(* Returns the number of nodes in graph g. *)
let graph_size g =
  int_size g.nodes
  ;;

(* Returns true iff node n is the destination of edge e *)
let is_dst n e =
  if e.dst = n then true else false
  ;;

(* Returns a list of edges in graph g whose source node is n *)
let src_edges n g =
  if graph_empty g = true then []
  else
  fold (fun a b -> if b.src = n then b::a else a) [] g.edges
  ;;


(* Returns the set of nodes reachable from node n in graph g, where the set is
  represented as an int_tree. If n is neither a source nor a destination in the
  graph, IntLeaf should be returned. *)
let reachable n g =
  if int_mem n g.nodes then
     let rec reachable_helper h =
        fold (fun a b ->
           if int_mem b.dst a then int_insert n a
           else if int_mem b.src a then reachable_helper (int_insert b.dst a)
           else if b.src = n then reachable_helper (int_insert b.dst a)
           else a) h g.edges
     in reachable_helper empty_int_tree
  else empty_int_tree
  ;;
