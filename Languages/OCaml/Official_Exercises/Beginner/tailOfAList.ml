let rec last lst =
  match lst with
  | [] -> None
  | h :: [] -> Some h
  | h :: t -> last t
