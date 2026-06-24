let split lst len =
  let rec aux lst len fstAcc =
    if len = 0
    then (List.rev fstAcc, lst)
    else match lst with
         | [] -> (List.rev fstAcc, lst)
         | h :: t -> aux t (len - 1) (h :: fstAcc)
  in aux lst len []
;;
