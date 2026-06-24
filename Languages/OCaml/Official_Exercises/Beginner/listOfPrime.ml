let range lo hi =
  let rec aux lo hi acc =
    if lo > hi
    then List.rev acc
    else aux (lo + 1) hi (lo :: acc)
  in aux lo hi []
;;


let all_primes lo hi =
  let rec sieve lst =
    match lst with
    | [] -> []
    | h :: t -> h :: (sieve (List.filter (fun x -> x mod h <> 0) t))
  in List.filter (fun x -> x >= lo) (sieve (range 2 hi))
;;
