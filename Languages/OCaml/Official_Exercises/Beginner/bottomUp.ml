(* We make the assumption that bottom up means post order traversal *)

type 'a multi_tree =
  | T of 'a * ('a multi_tree) list
;;

let rec bottom_up t =
  match t with
  | T (x, lst) -> List.fold_right (@) (List.map bottom_up lst) [x]
;;

(* The solution from the OCaml website seems to be identical in spirit *)
(* However, a more interesting interpretation of the exercise is if every *)
(* element on a lower level have to come before the elements on the higher *)
(* level *)

let bottom_up_by_level t =
  let rec merge l1 l2 =
    match l1, l2 with
    | [], l2 -> l2
    | l1, [] -> l1
    | (h1 :: t1), (h2 :: t2) -> (h1 @ h2) :: merge t1 t2
  in
  let rec aux = function
    | T (x, lst) -> [x] :: (List.fold_right merge (List.map aux lst) [])
  in
  List.fold_left (fun acc level -> level @ acc) [] (aux t)
;;
