type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

let collect_leaves t =
  let rec aux t acc =
    match t with
    | Empty -> acc
    | Node (elem, Empty, Empty) -> elem :: acc
    | Node (_, l, r) -> aux l acc |> aux r
  in aux t []
;;
