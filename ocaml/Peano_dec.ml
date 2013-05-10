(** val coq_O_or_S : int -> int option **)

let coq_O_or_S n =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    None)
    (fun n0 -> Some
    n0)
    n

