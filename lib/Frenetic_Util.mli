open Core.Std 

module StringSet : Set.S with type Elt.t = string

val make_string_of : (Format.formatter -> 'a -> 'b) -> 'a -> string

val string_of_option : ('a -> string) -> 'a option -> string

val string_of_list : ('a -> string) -> 'a list -> string
