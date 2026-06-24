let coprime x y =
  let rec euclidean hi lo =
    if lo = 0
    then hi
    else euclidean lo (hi mod lo)
  in
  let gcd = if x >= y
            then euclidean x y
            else euclidean y x
  in gcd = 1
;;
