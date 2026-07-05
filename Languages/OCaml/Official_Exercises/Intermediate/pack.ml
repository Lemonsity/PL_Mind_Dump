open List

let pack l =
  let rec aux l curr acc =
    match l with
    | [] -> acc
    | [x] -> (x :: curr) :: acc
    | h1 :: h2 :: t ->
      if h1 = h2
      then aux (h2 :: t) (h1 :: curr) acc
      else aux (h2 :: t) [] ((h1 :: curr) :: acc)
  in aux l [] [] |> rev
;;
