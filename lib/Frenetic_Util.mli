open Core.Std 

module IntPairTbl : Hashtbl.S with type key = (int * int)

val make_string_of : (Format.formatter -> 'a -> 'b) -> 'a -> string

val string_of_option : ('a -> string) -> 'a option -> string

val string_of_list : ('a -> string) -> 'a list -> string
