(* open Types *)

type 'a coq_Wildcard =
| WildcardExact of 'a
| WildcardAll
| WildcardNone

(** val coq_Wildcard_rect :
    ('a1 -> 'a2) -> 'a2 -> 'a2 -> 'a1 coq_Wildcard -> 'a2 **)

let coq_Wildcard_rect f f0 f1 = function
| WildcardExact x -> f x
| WildcardAll -> f0
| WildcardNone -> f1

(** val coq_Wildcard_rec :
    ('a1 -> 'a2) -> 'a2 -> 'a2 -> 'a1 coq_Wildcard -> 'a2 **)

let coq_Wildcard_rec f f0 f1 = function
| WildcardExact x -> f x
| WildcardAll -> f0
| WildcardNone -> f1

module Wildcard = 
 struct 
  (** val inter :
      'a1 coq_Eqdec -> 'a1 coq_Wildcard -> 'a1 coq_Wildcard -> 'a1
      coq_Wildcard **)

  let eq_dec eqdec x y =
    match x with
    | WildcardExact x0 ->
      (match y with
       | WildcardExact a0 -> eqdec x0 a0
       | _ -> false)
    | WildcardAll ->
      (match y with
       | WildcardAll -> true
       | _ -> false)
    | WildcardNone ->
      (match y with
       | WildcardNone -> true
       | _ -> false)  

  let inter eqdec x y =
    match x with
    | WildcardExact m ->
      (match y with
       | WildcardExact n ->
         if eqdec m n then WildcardExact m else WildcardNone
       | WildcardAll -> x
       | WildcardNone -> WildcardNone)
    | WildcardAll ->
      (match y with
       | WildcardNone -> WildcardNone
       | _ -> y)
    | WildcardNone -> WildcardNone
  
  (** val is_all : 'a1 coq_Wildcard -> bool **)
  
  let is_all = function
  | WildcardAll -> true
  | _ -> false
  
  (** val is_empty : 'a1 coq_Wildcard -> bool **)
  
  let is_empty = function
  | WildcardNone -> true
  | _ -> false
  
  (** val is_exact : 'a1 coq_Wildcard -> bool **)
  
  let is_exact = function
  | WildcardExact a -> true
  | _ -> false
    
  (** val to_option : 'a1 coq_Wildcard -> 'a1 option **)
  
  let to_option = function
  | WildcardExact a -> Some a
  | WildcardAll -> None
  | WildcardNone -> assert false (* absurd case *)
 end

