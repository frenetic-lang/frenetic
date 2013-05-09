open Types

type 'a coq_Wildcard =
| WildcardExact of 'a
| WildcardAll
| WildcardNone

val coq_Wildcard_rect : ('a1 -> 'a2) -> 'a2 -> 'a2 -> 'a1 coq_Wildcard -> 'a2

val coq_Wildcard_rec : ('a1 -> 'a2) -> 'a2 -> 'a2 -> 'a1 coq_Wildcard -> 'a2

module Wildcard : 
 sig 
  val inter :
    'a1 coq_Eqdec -> 'a1 coq_Wildcard -> 'a1 coq_Wildcard -> 'a1 coq_Wildcard
  
  val is_all : 'a1 coq_Wildcard -> bool
  
  val is_empty : 'a1 coq_Wildcard -> bool
  
  val is_exact : 'a1 coq_Wildcard -> bool
  
  val eq_dec : 'a1 coq_Eqdec -> 'a1 coq_Wildcard -> 'a1 coq_Wildcard -> bool
  
  val to_option : 'a1 coq_Wildcard -> 'a1 option
 end

