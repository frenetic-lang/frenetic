type 'a t

val map : ('a -> 'b) -> 'a t -> 'b t
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val now : 'a t -> 'a
val constant : 'a -> 'a t

(** [from_stream init stream = (puller, node)] returns an Lwt
    computation, [puller] that pulls values from the stream, and an FRP
    node, [node] holding the value of the stream. Run the [puller]
    computation to propagate values to [node] and its dependents. If
    value-propagation signals an exception, it will be raised as an Lwt
    exception. *)
val from_stream : 'a -> 'a Lwt_stream.t -> unit Lwt.t * 'a t
val to_stream : 'a t -> 'a Lwt_stream.t
