open List;;

let rand_select (l : 'a list) (n : int) : 'a list =
  Random.init 0;
  let rec extract (l : 'a list) (i : int) (acc : 'a list) : ('a * 'a list) =
    match l with
    | [] -> raise (Failure "Index out of range")
    | h :: t when i = 0 -> (h, rev_append acc t)
    | h :: t -> extract t (i - 1) (h :: acc)
  in
  let rec choose (l : 'a list) (len : int) (n : int) (acc : 'a list) : 'a list =
    if n = 0
    then acc
    else
      let i = Random.int len in
      let elem, rest = extract l i [] in
      choose rest (len - 1) (n - 1) (elem :: acc)
  in choose l (length l) n []
;;


(* This is an attempt to write an efficient random select algorithm *)
(* It is unfortunate the algorithm does not implement uniform distribution *)
(* Consider the case where l = [1;2;3;4], n = 2 *)
(* If the first index is chosen to be 2, the second index will force to be 4 *)
(* Thus there is a 1/3 probability for the program to choose the subset [3;4] *)

(* Still, I think there are interesting lessons to be taken *)
(* The algorithm runs in linear time, always *)
(* The algorithm produces a subseq of the original pool *)

let rand_select_alt (l : 'a list) (n : int) =
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
