let rec pullout_at k lst acc =  (* There is an implicit assumption that the index is in range *)
  match lst with
  | h :: t -> if k = 0
              then h, ((List.rev acc) @ t)
              else pullout_at (k - 1) t (h :: acc)
;;

let permutation lst =
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | _ -> let k = Random.int (List.length lst) in
           let (elem, rest) = pullout_at k lst []
           in aux rest (elem :: acc)
  in aux lst []
;;
