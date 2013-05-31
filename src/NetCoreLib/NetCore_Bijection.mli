type 'a t

val create : int -> 'a t

val add : 'a t -> 'a -> 'a -> unit

val del : 'a t -> 'a -> unit

val lookup : 'a t -> 'a -> 'a option

val members : 'a t -> 'a list
