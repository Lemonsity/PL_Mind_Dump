type 'a stream =
  | Cell of 'a * (unit -> 'a stream)

let hd : 'a stream -> 'a = function
  | Cell (h, _) -> h
;;
let tl : 'a stream -> 'a stream = function
  | Cell (_, t) -> t ()
;;
let rec take (n : int) (seq : 'a stream) : 'a list =
  if n = 0
  then []
  else
    let Cell (h, t) = seq
    in h :: take (n - 1) (t ())
;;
let rec unfold (create : 'a -> 'b * 'a) (init : 'a) : 'b stream =
  let (elem, new_init) = create init
  in Cell (elem, fun _ -> unfold create new_init)
;;
let rec bang (elem : 'a) : 'a stream =
  Cell (elem, fun _ -> bang elem)
;;
let rec ints (init : int) : int stream =
  unfold (fun x -> (x, x + 1)) init
;;
let rec map (f : 'a -> 'b) (seq : 'a stream) : 'b stream =
  let Cell (elem, t) = seq
  in Cell (f elem, fun () -> map f (t ()))
;;
let rec filter (p : 'a -> bool) (seq : 'a stream) : 'a stream =
  let Cell (elem, t) = seq in
  if p elem
  then Cell (elem, fun () -> filter p (t ()))
  else filter p (t ())
;;
let rec iter (execute : 'a -> unit) (seq : 'a stream) : 'b =
  let Cell (elem, t) = seq in
  begin
    execute elem ;
    iter execute (t ())
  end
;;
let rec to_seq (seq : 'a stream) : 'a Seq.t =
  let Cell (h, t) = seq in
  fun () -> Seq.Cons (h, to_seq (t ()))
;;
let rec of_seq (seq : 'a Seq.t) : 'a stream =
  match seq () with
  | Seq.Nil -> failwith "Bad sequence"
  | Seq.Cons (h, t) -> Cell (h, fun () -> of_seq t)
;;
