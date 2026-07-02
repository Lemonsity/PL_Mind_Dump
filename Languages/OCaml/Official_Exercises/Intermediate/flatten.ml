open List ;;
open Fun ;;

type 'a node =
  | One of 'a
  | Many of 'a node list
;;

let flatten (node_list : 'a node list) : 'a list =
  let rec aux (node : 'a node) acc =
    match node with
    | One x -> x :: acc
    | Many lst -> fold_left (flip aux) acc lst
  in rev (aux (Many node_list) [])
;;
