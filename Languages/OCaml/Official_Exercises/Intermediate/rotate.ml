open List;;

type exn += ImpossibleIndex of string;;

let rotate (l : 'a list) (n : int) : 'a list =
  let canonical_n = n mod length l in
  let rec aux l index acc =
    match l with
    | [] -> raise (ImpossibleIndex "Rotate should be less than length")
    | h :: t when index < canonical_n -> aux t (succ index) (h :: acc)
    | h :: t as l -> (rev acc, l)
  in
  let (front, back) = aux l 0 []
  in back @ front
;;
