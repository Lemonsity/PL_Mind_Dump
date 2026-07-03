open List

let compress l =
  let rec aux l acc =
    match l with
    | [] -> acc
    | [x] -> x :: acc
    | h1 :: h2 :: t ->
      if h1 = h2
      then aux (h2 :: t) acc
      else aux (h2 :: t) (h1 :: acc)
  in aux l [] |> rev
;;
