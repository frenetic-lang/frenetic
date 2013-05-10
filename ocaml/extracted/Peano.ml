(** val pred : int -> int **)

let pred = fun n -> max 0 (n-1)

(** val plus : int -> int -> int **)

let rec plus = (+)

(** val mult : int -> int -> int **)

let rec mult = ( * )

(** val minus : int -> int -> int **)

let rec minus = fun n m -> max 0 (n-m)

(** val nat_iter : int -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)

let rec nat_iter n f x =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    x)
    (fun n' ->
    f (nat_iter n' f x))
    n

