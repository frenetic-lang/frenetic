(** [Lwt_channel] is a cleaner than [Lwt_stream]. *)

type 'a t

(** [of_pushed_stream stream push] creates a channel from an [Lwt_stream] and
    its push function. The type system cannot ensure that [push] actually
    pushes values onto [stream], so use with care. *)
val of_pushed_stream : 'a Lwt_stream.t -> ('a option -> unit) -> 'a t

val create : unit -> 'a t

val send : 'a -> 'a t -> unit Lwt.t

val recv : 'a t -> 'a Lwt.t

val to_stream : 'a t -> 'a Lwt_stream.t
