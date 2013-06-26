module Log : sig

  val set_log_file : bool -> string -> unit

  val get_log_chan : unit -> out_channel

  val printf : ('a, out_channel, unit) format -> 'a

end

(** [Lwt_channel] is a cleaner than [Lwt_stream]. *)
module Lwt_channel : sig

  type 'a t

  (** [of_pushed_stream stream push] creates a channel from an [Lwt_stream] and
      its push function. The type system cannot ensure that [push] actually
      pushes values onto [stream], so use with care. *)
  val of_pushed_stream : 'a Lwt_stream.t -> ('a option -> unit) -> 'a t

  val create : unit -> 'a t

  val send : 'a -> 'a t -> unit Lwt.t
    
  val recv : 'a t -> 'a Lwt.t
    
  val to_stream : 'a t -> 'a Lwt_stream.t

end
  

val test_bit : int -> Int32.t -> bool
val clear_bit : int -> Int32.t -> Int32.t
val set_bit : int -> Int32.t -> Int32.t
val bit : Int32.t -> int -> bool -> Int32.t
val mac_of_bytes : string -> Int64.t
val get_byte : Int64.t -> int -> int
val string_of_mac : Int64.t -> string
val bytes_of_mac : Int64.t -> string

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
val intersperse : 'a -> 'a list -> 'a list

module type SAFESOCKET = sig
  type t = Lwt_unix.file_descr
  val create : Lwt_unix.file_descr -> t
  val recv : t -> string -> int -> int -> bool Lwt.t
end

module SafeSocket : SAFESOCKET
