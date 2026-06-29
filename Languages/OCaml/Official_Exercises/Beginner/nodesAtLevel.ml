type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree
;;

let at_level t n =
  let rec aux t n acc =
    match t with
    | Empty -> acc
    | Node (x, l, r) ->
      if n = 1
      then x :: acc
      else aux r (n-1) (aux l (n-1) acc)
  in aux t n []
;;
