open Datatypes
open List0

type 'a coq_Eqdec = 'a -> 'a -> bool

(** val second : ('a2 -> 'a3) -> ('a1 * 'a2) -> 'a1 * 'a3 **)

let second f = function
| (a, b) -> (a, (f b))

type 'a coq_Eq =
  'a -> 'a -> bool
  (* singleton inductive, whose constructor was Build_Eq *)

(** val coq_Eq_rect : (('a1 -> 'a1 -> bool) -> 'a2) -> 'a1 coq_Eq -> 'a2 **)

let coq_Eq_rect f e =
  f e

(** val coq_Eq_rec : (('a1 -> 'a1 -> bool) -> 'a2) -> 'a1 coq_Eq -> 'a2 **)

let coq_Eq_rec f e =
  f e

(** val eqdec : 'a1 coq_Eq -> 'a1 -> 'a1 -> bool **)

let eqdec eq =
  eq

(** val beqdec : 'a1 coq_Eq -> 'a1 -> 'a1 -> bool **)

let beqdec e x y =
  if eqdec e x y then true else false

(** val coq_Eq_nat : int coq_Eq **)

let coq_Eq_nat =
  (=)

(** val option_eq : 'a1 coq_Eq -> 'a1 option -> 'a1 option -> bool **)

let option_eq e x y =
  match x with
  | Some x0 ->
    (match y with
     | Some a1 -> eqdec e x0 a1
     | None -> false)
  | None ->
    (match y with
     | Some a0 -> false
     | None -> true)

(** val coq_Eq_option : 'a1 coq_Eq -> 'a1 option coq_Eq **)

let coq_Eq_option e =
  option_eq e

(** val pair_eq :
    'a1 coq_Eq -> 'a2 coq_Eq -> ('a1 * 'a2) -> ('a1 * 'a2) -> bool **)

let pair_eq eqA eqB x y =
  let (x0, x1) = x in
  let (a0, b0) = y in if eqdec eqA x0 a0 then eqdec eqB x1 b0 else false

(** val coq_Eq_pair : 'a1 coq_Eq -> 'a2 coq_Eq -> ('a1 * 'a2) coq_Eq **)

let coq_Eq_pair eA eB =
  pair_eq eA eB

(** val list_eq : 'a1 coq_Eq -> 'a1 list -> 'a1 list -> bool **)

let list_eq = (fun _ x y -> x = y)

(** val coq_Eq_list : 'a1 coq_Eq -> 'a1 list coq_Eq **)

let coq_Eq_list e =
  list_eq e

(** val concat_map : ('a1 -> 'a2 list) -> 'a1 list -> 'a2 list **)

let concat_map f lst =
  fold_right (fun a bs -> app (f a) bs) [] lst

(** val filter_map_body :
    ('a1 -> 'a2 option) -> 'a1 -> 'a2 list -> 'a2 list **)

let filter_map_body f a bs =
  match f a with
  | Some b -> b :: bs
  | None -> bs

(** val filter_map : ('a1 -> 'a2 option) -> 'a1 list -> 'a2 list **)

let filter_map f lst =
  fold_right (filter_map_body f) [] lst

(** val intersperse : 'a1 -> 'a1 list -> 'a1 list **)

let intersperse v lst =
  fold_right (fun x xs -> x :: (v :: xs)) [] lst

