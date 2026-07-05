open List;;

let slice (l : 'a list) (i : int) (k : int) =
  let diff = k - i + 1 in
  if diff <= 0
  then []
  else drop i l |> take diff
;;
