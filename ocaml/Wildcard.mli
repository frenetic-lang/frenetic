(* open Types *)

type 'a coq_Wildcard =
| WildcardExact of 'a
| WildcardAll
| WildcardNone

module Wildcard : 
 sig 
  val eq_dec : ('a1 -> 'a1 -> bool) -> 'a1 coq_Wildcard -> 'a1 coq_Wildcard -> bool

  val inter : ('a1 -> 'a1 -> bool) -> 'a1 coq_Wildcard -> 'a1 coq_Wildcard -> 'a1 coq_Wildcard
  
  val is_all : 'a1 coq_Wildcard -> bool
  
  val is_empty : 'a1 coq_Wildcard -> bool
  
  val is_exact : 'a1 coq_Wildcard -> bool
  
  val to_option : 'a1 coq_Wildcard -> 'a1 option
 end

