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

module Make (Ord : OrderedType) = struct

  type a = Ord.t

  type t = 
    | WildcardExact of a
    | WildcardAll
    | WildcardNone

  let is_equal x y = match (x, y) with
    | WildcardExact a, WildcardExact b -> Ord.compare a b = 0
    | WildcardAll, WildcardAll -> true
    | WildcardNone, WildcardNone -> true
    | _ -> false

  let inter x y = match (x, y) with
    | WildcardNone, _ -> WildcardNone
    | _, WildcardNone -> WildcardNone
    | WildcardAll, y -> y
    | x, WildcardAll -> x
    | WildcardExact a, WildcardExact b -> 
      if Ord.compare a b = 0 then
        WildcardExact b
      else 
        WildcardNone

  let is_all x = match x with
    | WildcardAll -> true
    | _ -> false

  let is_empty x = match x with
    | WildcardNone -> true
    | _ -> false


  let is_exact x = match x with
    | WildcardExact _ -> true
    | _ -> false

  let to_option x = match x with
    | WildcardAll -> Some None
    | WildcardExact a -> Some (Some a)
    | WildcardNone -> None


  let to_string x = match x with
    | WildcardExact a -> "WildcardExact " ^ Ord.to_string a
    | WildcardNone -> "WildcardNone"
    | WildcardAll -> "WildcardAll"

end
