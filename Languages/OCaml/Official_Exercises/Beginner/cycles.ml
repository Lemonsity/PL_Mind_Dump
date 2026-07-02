open List

let rec lookup key map =
  match map with
    | [] -> None
    | (k, v) :: t ->
      if k = key
      then Some v
      else lookup key t
;;

let cycles g start =
  let rec discover history current =
    if exists ( (=) current ) history
    then
      if start = current
      then [(current :: history)]
      else []
    else
      match lookup current g with
      | None -> []
      | Some reachables ->
        map
          (discover (current :: history))
          reachables
        |> concat
  in discover [] start
  (* match lookup start g with *)
  (* | None -> [] *)
  (* | Some reachables -> map (discover [start]) reachables |> concat *)
;;

let cycles_tail g start =
  let rec discover history current ps =
    if history <> [] && start = current
    then (current :: history) :: ps
    else
      if exists ( (=) current ) history
      then ps
      else
        match lookup current g with
        | None -> ps
        | Some reachables -> fold_left
                               (fun ps next -> discover (current :: history) next ps)
                               ps
                               reachables
  in discover [] start []
;;
