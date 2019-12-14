open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let is_over_x x lst =
map (fun a -> if a > x then true else false) lst
;;

let count_over_x x lst = 
  fold (fun a h -> if h > x then a + 1 else a) 0 lst
;;

let mean lst = 
  if lst = [] then raise (Invalid_argument("mean"))
  else 
  let all, size =
    List.fold_left
      (fun (tot,len) x -> (x +. tot), (len +. 1.))
      (0., 0.) lst
  in
  (all /.size)
;;



let helperList lst = 
  let inner h t = t :: h in
	let outer h t = fold inner h t in
	let temp = fold outer [] lst in
  rev temp
  ;;
  
let  pred_succ lst = 
  let new_list = map (fun x -> [x-1;x;x+1]) lst in
  helperList new_list
  ;;


let bind f lst = 
  let new_list = map (fun a -> f a) lst in
	helperList new_list
;;
let ap fns args = 
  let temp_arg = rev args in
  let folded = fold (fun a f -> (map f temp_arg) :: a) [] fns in
  let temp = helperList folded in
  rev temp
  ;;
