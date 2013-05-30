val intercalate : ('a -> string) -> string -> 'a list -> string

val intersperse : 'a -> 'a list -> 'a list

val concat_map : ('a -> 'b list) -> 'a list -> 'b list

val filter_map : ('a -> 'b option) -> 'a list -> 'b list

val filter_none : ('a option) list -> 'a list
