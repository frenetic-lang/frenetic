type ('a,'b) sum =
  | Inl of 'a
  | Inr of 'b

(* val test_bit : int -> Int32.t -> bool *)
(* val clear_bit : int -> Int32.t -> Int32.t *)
(* val set_bit : int -> Int32.t -> Int32.t *)
(* val bit : Int32.t -> int -> bool -> Int32.t *)
(* val mac_of_bytes : string -> Int64.t *)
(* val get_byte : Int64.t -> int -> int *)
(* val string_of_mac : Int64.t -> string *)
(* val bytes_of_mac : Int64.t -> string *)
(* val string_of_ip : Int32.t -> string *)

(* val filter_map : ('a -> 'b option) -> 'a list -> 'b list *)
(* val concat_map : ('a -> 'b list) -> 'a list -> 'b list *)
val intersperse : 'a -> 'a list -> 'a list

val string_of_list : ('a -> string) -> 'a list -> string
val string_of_option : ('a -> string) -> 'a option -> string
val string_of_pair : ('a -> string) -> ('b -> string) -> ('a * 'b) -> string

val is_some : 'a option -> bool
val is_none : 'a option -> bool
