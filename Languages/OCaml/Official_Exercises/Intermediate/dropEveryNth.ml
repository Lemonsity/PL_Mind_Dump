open List;;

let drop (l : 'a list) (n : int) : 'a list =
  let rec aux (l : 'a list) (remain : int) (acc : 'a list) : 'a list =
    match l with
    | [] -> acc
    | h :: t -> if remain = 1
      then aux t n acc
      else aux t (remain - 1) (h :: acc)
  in aux l n [] |> rev
;;
