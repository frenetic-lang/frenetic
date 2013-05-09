open Datatypes
open Specif

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val hd : 'a1 -> 'a1 list -> 'a1 **)

let hd default = function
| [] -> default
| x :: l0 -> x

(** val hd_error : 'a1 list -> 'a1 option **)

let hd_error = function
| [] -> error
| x :: l0 -> value x

(** val tl : 'a1 list -> 'a1 list **)

let tl = function
| [] -> []
| a :: m -> m

module ListNotations = 
 struct 
  
 end

(** val destruct_list : 'a1 list -> ('a1, 'a1 list) sigT option **)

let destruct_list = fun _ -> failwith "destruct_list axiom"

(** val in_dec : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool **)

let rec in_dec h a = function
| [] -> false
| y :: l0 -> let s = h y a in if s then true else in_dec h a l0

(** val nth : int -> 'a1 list -> 'a1 -> 'a1 **)

let rec nth n l default =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    match l with
    | [] -> default
    | x :: l' -> x)
    (fun m ->
    match l with
    | [] -> default
    | x :: t -> nth m t default)
    n

(** val nth_ok : int -> 'a1 list -> 'a1 -> bool **)

let rec nth_ok n l default =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    match l with
    | [] -> false
    | x :: l' -> true)
    (fun m ->
    match l with
    | [] -> false
    | x :: t -> nth_ok m t default)
    n

(** val nth_in_or_default : int -> 'a1 list -> 'a1 -> bool **)

let nth_in_or_default = fun _ _ _ -> failwith "nth_in_or_default axiom"

(** val nth_error : 'a1 list -> int -> 'a1 coq_Exc **)

let rec nth_error l n =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    match l with
    | [] -> error
    | x :: l0 -> value x)
    (fun n0 ->
    match l with
    | [] -> error
    | a :: l0 -> nth_error l0 n0)
    n

(** val nth_default : 'a1 -> 'a1 list -> int -> 'a1 **)

let nth_default default l n =
  match nth_error l n with
  | Some x -> x
  | None -> default

(** val remove : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list **)

let rec remove eq_dec x = function
| [] -> []
| y :: tl0 ->
  if eq_dec x y then remove eq_dec x tl0 else y :: (remove eq_dec x tl0)

(** val last : 'a1 list -> 'a1 -> 'a1 **)

let rec last l d =
  match l with
  | [] -> d
  | a :: l0 ->
    (match l0 with
     | [] -> a
     | a0 :: l1 -> last l0 d)

(** val removelast : 'a1 list -> 'a1 list **)

let rec removelast = function
| [] -> []
| a :: l0 ->
  (match l0 with
   | [] -> []
   | a0 :: l1 -> a :: (removelast l0))

(** val exists_last : 'a1 list -> ('a1 list, 'a1) sigT **)

let exists_last = fun _ -> failwith "exists_last axiom"

(** val count_occ : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 -> int **)

let rec count_occ eq_dec l x =
  match l with
  | [] -> 0
  | y :: tl0 ->
    let n = count_occ eq_dec tl0 x in if eq_dec y x then succ n else n

(** val rev : 'a1 list -> 'a1 list **)

let rec rev = function
| [] -> []
| x :: l' -> app (rev l') (x :: [])

(** val rev_append : 'a1 list -> 'a1 list -> 'a1 list **)

let rec rev_append l l' =
  match l with
  | [] -> l'
  | a :: l0 -> rev_append l0 (a :: l')

(** val rev' : 'a1 list -> 'a1 list **)

let rev' l =
  rev_append l []

(** val list_eq_dec :
    ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool **)

let rec list_eq_dec eq_dec l l' =
  match l with
  | [] ->
    (match l' with
     | [] -> true
     | a :: l0 -> false)
  | y :: l0 ->
    (match l' with
     | [] -> false
     | a0 :: l1 -> if eq_dec y a0 then list_eq_dec eq_dec l0 l1 else false)

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| a :: t -> (f a) :: (map f t)

(** val flat_map : ('a1 -> 'a2 list) -> 'a1 list -> 'a2 list **)

let rec flat_map f = function
| [] -> []
| x :: t -> app (f x) (flat_map f t)

(** val fold_left : ('a1 -> 'a2 -> 'a1) -> 'a2 list -> 'a1 -> 'a1 **)

let rec fold_left f l a0 =
  match l with
  | [] -> a0
  | b :: t -> fold_left f t (f a0 b)

(** val fold_right : ('a2 -> 'a1 -> 'a1) -> 'a1 -> 'a2 list -> 'a1 **)

let rec fold_right f a0 = function
| [] -> a0
| b :: t -> f b (fold_right f a0 t)

(** val list_power : 'a1 list -> 'a2 list -> ('a1 * 'a2) list list **)

let rec list_power l l' =
  match l with
  | [] -> [] :: []
  | x :: t ->
    flat_map (fun f -> map (fun y -> (x, y) :: f) l') (list_power t l')

(** val existsb : ('a1 -> bool) -> 'a1 list -> bool **)

let rec existsb f = function
| [] -> false
| a :: l0 -> (||) (f a) (existsb f l0)

(** val forallb : ('a1 -> bool) -> 'a1 list -> bool **)

let rec forallb f = function
| [] -> true
| a :: l0 -> (&&) (f a) (forallb f l0)

(** val filter : ('a1 -> bool) -> 'a1 list -> 'a1 list **)

let rec filter f = function
| [] -> []
| x :: l0 -> if f x then x :: (filter f l0) else filter f l0

(** val find : ('a1 -> bool) -> 'a1 list -> 'a1 option **)

let rec find f = function
| [] -> None
| x :: tl0 -> if f x then Some x else find f tl0

(** val partition : ('a1 -> bool) -> 'a1 list -> 'a1 list * 'a1 list **)

let rec partition f = function
| [] -> ([], [])
| x :: tl0 ->
  let (g, d) = partition f tl0 in
  if f x then ((x :: g), d) else (g, (x :: d))

(** val split : ('a1 * 'a2) list -> 'a1 list * 'a2 list **)

let rec split = function
| [] -> ([], [])
| p :: tl0 ->
  let (x, y) = p in let (g, d) = split tl0 in ((x :: g), (y :: d))

(** val combine : 'a1 list -> 'a2 list -> ('a1 * 'a2) list **)

let rec combine l l' =
  match l with
  | [] -> []
  | x :: tl0 ->
    (match l' with
     | [] -> []
     | y :: tl' -> (x, y) :: (combine tl0 tl'))

(** val list_prod : 'a1 list -> 'a2 list -> ('a1 * 'a2) list **)

let rec list_prod l l' =
  match l with
  | [] -> []
  | x :: t -> app (map (fun y -> (x, y)) l') (list_prod t l')

(** val firstn : int -> 'a1 list -> 'a1 list **)

let rec firstn n l =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    [])
    (fun n0 ->
    match l with
    | [] -> []
    | a :: l0 -> a :: (firstn n0 l0))
    n

(** val skipn : int -> 'a1 list -> 'a1 list **)

let rec skipn n l =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    l)
    (fun n0 ->
    match l with
    | [] -> []
    | a :: l0 -> skipn n0 l0)
    n

(** val seq : int -> int -> int list **)

let rec seq start len =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    [])
    (fun len0 ->
    start :: (seq (succ start) len0))
    len

(** val coq_Forall_rect :
    'a2 -> ('a1 -> 'a1 list -> __ -> 'a2) -> 'a1 list -> 'a2 **)

let coq_Forall_rect h h' = function
| [] -> h
| y :: l0 -> h' y l0 __

