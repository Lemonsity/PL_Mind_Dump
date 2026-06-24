let insert_at sym k lst =
  let rec reverse_append lst1 lst2 =
    match lst1 with
    | [] -> lst2
    | h :: t -> reverse_append t (h :: lst2)
  in
  let rec aux sym k lst acc =   (* acc is the front of the list *)
    if k = 0
    then reverse_append acc (sym :: lst)
    else match lst with
         | [] -> reverse_append acc [sym]
         | h :: t -> aux sym (k - 1) t (h :: acc)
  in aux sym k lst []
;;
