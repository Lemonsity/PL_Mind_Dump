let rec at n lst =
  match lst with
  | [] -> None
  | h :: t when n == 0 -> h
  | h :: t -> at (n - 1) t
