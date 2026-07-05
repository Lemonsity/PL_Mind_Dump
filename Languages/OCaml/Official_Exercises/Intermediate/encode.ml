open List;;

type 'a rle =
  | One of 'a
  | Many of int * 'a
;;

let encode lst =
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
  in (aux lst 0 []) |> rev
;;
