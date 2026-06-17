let rec length lst =
  match lst with
  | [] -> 0
  | _ :: t -> length t + 1
;;


let lengthOpt lst =
  let rec lengthTail lst acc =
    match lst with
    | [] -> acc
    | _ :: t -> lengthTail t (acc + 1) in
  lengthTail lst 0
;;
