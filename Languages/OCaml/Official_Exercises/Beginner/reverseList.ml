let rev lst =
  let rec revAux lst acc =
    match lst with
    | [] -> acc
    | h :: t -> revAux t (h :: acc)
  in revAux lst []
;;
