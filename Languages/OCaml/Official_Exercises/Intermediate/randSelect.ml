open List;;

(* This is an attempt to write an efficient random select algorithm *)

(* Picking n elements out of a list is equivalent to picking n distinct,
   valid indices. *)
(* This algorithm randomly generates the difference between the indices *)
(* In theory, this algorithm will run in O(n + m), where (m = len l) *)
(* Furthermore, the output is a subseq of the list that represents the pool *)

(* I need some help proving this algorithm indeed follow the uniform distribution *)

let rand_select (l : 'a list) (n : int) =
  Random.init 0;
  let rec choose (remain_len : int) (to_select : int) (acc : int list) : int list =
    match remain_len < to_select, to_select = 0 with
    | true, _ -> raise (Failure "Not enough elements")
    | false, true -> acc
    | false, false ->
      let min, max = 0, remain_len - to_select in
      let index = Random.int_in_range ~min ~max in
      choose (remain_len - index - 1) (to_select - 1) (index :: acc)
  in
  let diffs = choose (length l) n [] |> rev in
  let rec collect (l : 'a list) (diffs : int list) (acc : 'a list) : 'a list =
    match diffs with
    | [] -> acc
    | j :: js ->
      let after_drop = drop j l in
      match after_drop with
      | [] -> raise (Failure "There are no elements left")
      | h :: t -> collect t js (h :: acc)
  in collect l diffs [] |> rev
;;
