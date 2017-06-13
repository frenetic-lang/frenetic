open Core
open Async

(** A node in a dynamic dataflow graph that receives values of type 'b and
 * produces values of type 'a. *)
type ('b, 'a) t

(** An uninhabited type used in the definition of [create_source] and
 * [from_pipe]. This is the type of values that these nodes receive, thus they
 * cannot receive any values.
 *)
type cannot_receive

(** [create init f] creates an isolated node, initialized to hold [init].
 * Use the [attach] function to connect the node to consumers and producers.
 * When any producer produces a value (of type ['b]), the function [f] is
 * applied to latest values of each produced to calculate the new value of
 * this node. The new value is sent to all the consumers of this node.
 *)
val create : 'a -> ('b list -> 'a) -> ('b, 'a) t

(** [create_source init] creates an isolated node initialized to hold [init].
 * You must not create an edge to this node--it will signal an error if it
 * receive a value. But, you can update the value at the node using [push].
 *)
val create_source : 'a -> (cannot_receive, 'a) t

(** [push v src] sets the value at [src] to [v] and propagates the update
 * through the dataflow graph.
 *)
val push : 'a -> ('b, 'a) t -> unit

(** [attach src dst] sends values from [src] to [dst]. Be careful not to
 * create a cyclc. If you do, an infinite loop will occur while propagating
 * values. *)
val attach : ('a, 'b) t -> ('b, 'c) t -> unit

(** [from_pipe init reader] creates a node initialized to [init] that is
 * updated with values from [reader].
 *
 * Note: You cannot create an edge to this node, but you can create an edge
 * from this node to another that accepts values of type ['a]. *)
val from_pipe : 'a -> 'a Pipe.Reader.t -> (cannot_receive, 'a) t

(** [to_pipe src] returns the current value of [src] and a pipe that carries
 * all future values of [src]. *)
val to_pipe : ('b, 'a) t -> 'a * 'a Pipe.Reader.t

