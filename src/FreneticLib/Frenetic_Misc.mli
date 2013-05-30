val map_option: ('a -> 'b) -> 'a option -> 'b option
val string_of_list : ('a -> string) -> 'a list -> string
val string_of_option : ('a -> string) -> 'a option -> string
val string_of_pair : ('a -> string) -> ('b -> string) -> ('a * 'b) -> string
