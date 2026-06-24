let range lo hi =
  let rec aux lo hi acc =
    if lo > hi
    then List.rev acc
    else aux (lo + 1) hi (lo :: acc)
  in aux lo hi []
;;
