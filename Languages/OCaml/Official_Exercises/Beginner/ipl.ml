type 'a multi_tree =
  | T of 'a * ('a multi_tree) list
;;

let ipl t =
  let rec aux (level : int) = function
    | T (_, lst) -> level + List.fold_right (+) (List.map (aux (level + 1)) lst) 0
  in aux 1 t
;;
