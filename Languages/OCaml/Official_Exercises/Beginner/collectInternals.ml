type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

let internals t =
  let rec aux t acc =
    match t with
    | Empty -> acc
    | Node (_, Empty, Empty) -> acc
    | Node (x, l, r) -> x :: (aux l acc |> aux r)
  in aux t []
;;
