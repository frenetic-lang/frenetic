module type Wildcard = sig

  type a

  type t = 
    | WildcardExact of a
    | WildcardAll
    | WildcardNone

  val is_equal : t -> t -> bool
  val inter : t -> t -> t
  val is_all : t -> bool
  val is_empty : t -> bool
  val is_exact : t -> bool
  val to_option : t -> a option option
  val to_string : t -> string

end

module type OrderedType = sig
  type t 
  val compare : t -> t -> int
  val to_string : t -> string
end

module Make : functor (Ord : OrderedType) -> Wildcard 
  with type a = Ord.t
