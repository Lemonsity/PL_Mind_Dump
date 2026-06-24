let remove_at index lst =
  let rec reverse_append lst1 lst2 =
    match lst1 with
    | [] -> lst2
    | h :: t -> reverse_append t (h :: lst2)
  in
  let rec aux index lst acc =
    match lst with
    | [] -> List.rev acc
    | h :: t -> if index = 0
                then reverse_append acc t
                else aux (index - 1) t (h :: acc)
  in aux index lst []
;;
