open List;;

type 'a rle =
  | One of 'a
  | Many of int * 'a
;;

let decode (l : 'a rle list) =
  let rec replicate (n : int) (elem : 'a) (acc : 'a list) : 'a list =
    if n = 0 then acc else replicate (n - 1) elem (elem :: acc) in
  let rec aux (l : 'a rle list) (acc : 'a list) : 'a list =
    match l with
    | [] -> acc
    | (One elem) :: t -> aux t (elem :: acc)
    | (Many (n, elem)) :: t -> aux t (replicate n elem acc)
  in aux l [] |> rev
;;
