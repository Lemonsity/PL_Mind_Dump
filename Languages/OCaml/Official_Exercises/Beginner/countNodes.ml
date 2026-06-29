type 'a multi_tree =
  | T of 'a * ('a multi_tree) list
;;

let rec count_nodes_functional = function
  | T (x, lst) -> 1 + List.fold_right (+) (List.map count_nodes_functional lst) 0
;;
