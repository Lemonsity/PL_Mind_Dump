let rec encode = function
  | [] -> []
  | head :: tail -> match (encode tail) with
                    | [] -> [(1, head)]
                    | (cnt, sym) :: result_tail -> if head = sym
                                                   then (cnt + 1, sym) :: result_tail
                                                   else (1, head) :: (cnt, sym) :: result_tail
;;

let encodeTail lst =
  let rec aux cur_cnt acc lst =
    match lst with
    | [] -> acc
    | [head] -> (cur_cnt + 1, head) :: acc
    | head1 :: (head2 :: _ as tail) ->
       if head1 = head2
       then aux (cur_cnt + 1) acc tail
       else aux 0 ( (cur_cnt + 1, head1) :: acc) tail
  in List.rev (aux 0 [] lst)
;;

(* Above are taken from the simple run length count question *)

type 'a rle =
  | One of 'a
  | Many of int * 'a
;;

let encode_compose = Fun.compose
                  (List.map (fun (cnt, sym) -> if cnt == 1
                                               then One (sym)
                                               else Many (cnt, sym)))
                  encode
;;

let encode_raw lst =
  let make_tuple sym cnt = if cnt = 1
                           then One sym
                           else Many (cnt, sym)
  in
  let rec aux lst cnt acc =
    match lst with
    | [] -> []
    | [sym] -> (make_tuple sym (cnt + 1)) :: acc
    | fst :: (snd :: _ as tail) -> if fst = snd
                                   then aux tail (cnt + 1) acc
                                   else aux tail 0 ((make_tuple fst (cnt + 1)) :: acc)
  in List.rev (aux lst 0 [])
;;
