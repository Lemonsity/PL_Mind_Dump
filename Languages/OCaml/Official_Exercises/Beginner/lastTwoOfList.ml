let rec last_two lst =
  match lst with
  | [] -> None
  | car :: [] -> None
  | car :: cadr :: [] -> Some (car, cadr)
  | _ :: t -> last_two t
