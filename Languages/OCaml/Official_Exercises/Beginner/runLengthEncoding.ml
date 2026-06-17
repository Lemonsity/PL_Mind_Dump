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
