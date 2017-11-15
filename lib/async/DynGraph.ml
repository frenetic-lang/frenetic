open Core
open Async

type 'a consumer = 'a -> unit

type 'a producer = unit -> 'a

type cannot_receive

type ('b, 'a) t = {
  mutable now : 'a; (* the value at this node *)
  producer : unit -> 'a; (* produces the value at this node *)
  mutable to_list : ('a consumer) list; (* push updates to these consumers *)
  mutable from_list : ('b producer) list; (* combine the values from these producers *)
  receive : 'b -> unit (* recalculate at this node when this function is applied *)
}

let propagate (node: ('b, 'a) t) : unit =
  let v = node.now in
  List.iter node.to_list ~f:(fun f -> f v)

let create (init: 'a) (f: 'b list -> 'a) : ('b, 'a) t =
  let rec node = {
    now = init;
    producer = (fun () -> node.now);
    to_list = [];
    from_list = [];
    receive = (fun b ->
      node.now <- f (List.map node.from_list ~f:(fun f -> f ()));
      propagate node)
  } in
  node

let create_source (init: 'a) : (cannot_receive, 'a) t =
  let rec node = {
    now = init;
    producer = (fun () -> node.now);
    to_list = [];
    from_list = [];
    receive = (fun _ -> failwith "impossible: create_source node received a value")
  } in
  node

let push (x: 'a) (node : ('b, 'a) t) : unit =
    node.now <- x;
    propagate node

let attach (src : ('a, 'b) t) (dst : ('b, 'c) t) : unit =
  src.to_list <- dst.receive :: src.to_list;
  dst.from_list <- src.producer :: dst.from_list

let to_pipe (node : ('b, 'a) t) : 'a * 'a Pipe.Reader.t  =
  let (r, w) = Pipe.create () in
  let consume b = Pipe.write_without_pushback w b in
  node.to_list <- consume :: node.to_list;
  (node.producer (), r)

let from_pipe (init : 'a) (reader : 'a Pipe.Reader.t) : (cannot_receive, 'a) t =
  let rec node = {
    now = init;
    producer = (fun () -> node.now);
    to_list = [];
    from_list = [];
    receive = (fun _ -> failwith "impossible: from_pipe node received a value")
  } in
  let _ = Pipe.iter_without_pushback reader ~f:(fun x ->
    node.now <- x;
    List.iter node.to_list ~f:(fun f -> f x)) in
  node
