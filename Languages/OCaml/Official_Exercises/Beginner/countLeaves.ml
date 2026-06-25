type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

let rec count_leaves = function
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, l, r) -> count_leaves l + count_leaves r
;;
