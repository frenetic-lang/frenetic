open Core.Std
open Async.Std

open NetKAT_Types

exception Assertion_failed of string

type node =
    | Switch of SDN_Types.switchId
    | Host of Packet.dlAddr * Packet.nwAddr with sexp

module Node = struct
  type t = node with sexp

  let compare = Pervasives.compare

  let to_string t = match t with
    | Switch(sw_id)       -> Printf.sprintf "switch %Lu" sw_id
    | Host(dlAddr, nwAddr) -> Printf.sprintf "host %s/%s"
        (Packet.string_of_nwAddr nwAddr)
        (Packet.string_of_dlAddr dlAddr)

  let parse_dot _ _ = failwith "NYI: Node.parse_dot"
  let parse_gml _ = failwith "NYI: Node.parse_dot"

  let to_dot _ = failwith "NYI: Node.to_dot"
  let to_mininet _ = failwith "NYI: Node.to_mininet"
end

module Link = struct
  type t = unit with sexp

  let compare = Pervasives.compare

  let to_string () = "()"
  let default = ()

  let parse_dot _ = failwith "NYI: Link.parse_dot"
  let parse_gml _ = failwith "NYI: Link.parse_dot"

  let to_dot _ = failwith "NYI: Link.to_dot"
  let to_mininet _ = failwith "NYI: Link.to_mininet"
end

module Net = Network.Make(Node)(Link)
module PipeSet = Raw_app.PipeSet

(* Helper function to get around an issue with Pipe.transfer, reported here:
 *
 *   https://github.com/janestreet/async_kernel/issues/3
 *)
let transfer_batch r w ~f =
  Pipe.transfer' r w ~f:(fun q -> return (Queue.map q ~f))

exception Sequence_error of PipeSet.t * PipeSet.t

type ('phantom, 'a) pipes = {
  pkt_out : (switchId * SDN_Types.pktOut, 'phantom) Pipe.t;
  update  : ('a, 'phantom) Pipe.t
}

type 'a send = (Pipe.Writer.phantom, 'a) pipes
type 'a recv = (Pipe.Reader.phantom, 'a) pipes

type 'a callback = event -> 'a option Deferred.t

type ('r, 'a) handler  = 'r -> (switchId * SDN_Types.pktOut) Pipe.Writer.t -> unit -> 'a callback
type ('r, 'a) async_handler = 'r -> 'a send -> unit -> 'a callback

module Raw = struct
  type ('r, 'a) t = ('r, 'a) Raw_app.t

  let create ?pipes (value : 'a) (handler : ('r, 'a) handler) : ('r, 'a) t =
    let open Raw_app in
    create_primitive ?pipes value (fun r send () ->
      let callback = handler r send.pkt_out () in
      fun e ->
        callback e
        >>= function
          | None    -> Pipe.write send.update EventNoop
          | Some(p) -> Pipe.write send.update (Event p))

  let create_async ?pipes (value : 'a) (handler : ('r, 'a) async_handler) : ('r, 'a) t =
    let open Raw_app in
    create_primitive ?pipes value (fun r send () ->
      let r_update, w_update = Pipe.create () in
      Deferred.don't_wait_for
        (transfer_batch r_update send.update ~f:(fun p -> (Async p)));
      let callback = handler r { pkt_out = send.pkt_out; update = w_update } () in
      fun e ->
        callback e
        >>= function
          | None    -> Pipe.write send.update EventNoop
          | Some(p) -> Pipe.write send.update (Event p))

  let create_static (value : 'a) : ('r, 'a) t =
    Raw_app.create_static value

end

module Pred = struct
  type t = (Net.Topology.t ref, pred) Raw_app.t

  type handler = Net.Topology.t -> event -> pred option Deferred.t
  type async_handler = pred Pipe.Writer.t -> unit -> handler

  let create (pred : pred) (handler : handler) : t =
    let open Raw_app in
    create_primitive pred (fun nib send () ->
      Pipe.close send.pkt_out;
      fun e ->
        handler !nib e
        >>= function
          | None    -> Pipe.write send.update EventNoop
          | Some(p) -> Pipe.write send.update (Event p))

  let create_async (pred : pred) (handler : async_handler) : t =
    let open Raw_app in
    create_primitive pred (fun nib send () ->
      let r_update, w_update = Pipe.create () in
      Pipe.close send.pkt_out;
      Deferred.don't_wait_for
        (transfer_batch r_update send.update ~f:(fun p -> (Async p)));
      let callback = handler w_update () in
      fun e ->
        callback !nib e
        >>= function
          | None    -> Pipe.write send.update EventNoop
          | Some(p) -> Pipe.write send.update (Event p))

  let create_static (pred : pred) : t =
    Raw_app.create_static pred

  let create_from_string (str : string) : t =
    let pol = NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_string str) in
    match pol with
    | Filter(pred) -> create_static pred
    | _            -> assert false (* XXX(seliopou): raise exception *)

  let create_from_file (filename : string) : t =
    let pol = In_channel.with_file filename ~f:(fun chan ->
      NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_channel chan))
    in
    match pol with
    | Filter(pred) -> create_static pred
    | _            -> assert false (* XXX(seliopou): raise exception *)

end

module Policy = struct
  type t = (Net.Topology.t ref, policy) Raw.t

  let create ?pipes (policy : policy) (handler : (Net.Topology.t ref, policy) handler) : t =
    Raw.create ?pipes policy handler

  let create_async ?pipes (policy : policy) (handler : (Net.Topology.t ref, policy) async_handler) : t =
    Raw.create_async ?pipes policy handler

  let create_static (policy : policy) : t =
    Raw.create_static policy

  let create_from_string (str : string) : t =
    let pol = NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_string str) in
    create_static pol

  let create_from_file (filename : string) : t =
    let pol = In_channel.with_file filename ~f:(fun chan ->
      NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_channel chan)) in
    create_static pol
end


let default (t : ('r, 'a) Raw.t) : 'a =
  t.Raw_app.value

let lift (f : 'a -> 'b) (t : ('r, 'a) Raw.t) : ('r, 'b) Raw.t =
  Raw_app.lift f t

let ap ?(how=`Sequential) (t1 : ('r, 'a -> 'b) Raw.t) (t2 : ('r, 'a) Raw.t) : ('r, 'b) Raw.t =
  Raw_app.ap t1 t2

let run (t : ('r, 'a) Raw.t) (r : 'r) () : ('a recv * (event -> unit Deferred.t)) =
  let recv, callback = Raw_app.run t r () in
  { pkt_out = recv.Raw_app.pkt_out; update = recv.Raw_app.update }, callback

let neg (t:Pred.t) : Pred.t =
  Raw_app.lift Optimize.mk_not t

let conj (t1:Pred.t) (t2:Pred.t) : Pred.t =
  Raw_app.combine ~how:`Parallel Optimize.mk_and t1 t2

let disj (t1:Pred.t) (t2:Pred.t) : Pred.t =
  Raw_app.combine ~how:`Parallel Optimize.mk_or t1 t2

let union ?(how=`Parallel) (app1 : Policy.t) (app2 : Policy.t) : Policy.t =
  Raw_app.combine ~how:how Optimize.mk_union app1 app2

exception Sequence_error of PipeSet.t * PipeSet.t

let seq (app1 : Policy.t) (app2 : Policy.t) : Policy.t =
  let open Raw_app in
  begin if not PipeSet.(is_empty (inter app1.pipes app2.pipes)) then
    (* In order for the form of composition below, the apps must not be
     * listening on the same pipe for `PacketIn` events. In this case,
     * only one of the apps will actually run and produce PacketOut messages
     * on a `PacketIn` event. *)
    raise (Sequence_error(app1.pipes, app2.pipes))
  end;
  Raw_app.combine ~how:`Sequential Optimize.mk_seq app1 app2

let filter (p : pred) : Policy.t =
  Policy.create_static (Filter p)

let filter' (p : Pred.t) : Policy.t =
  lift (fun p -> Filter p) p

let guard (pred : pred) (app : Policy.t) : Policy.t =
  seq (filter pred) app

let guard' (pred : Pred.t) (app : Policy.t) : Policy.t =
  seq (filter' pred) app

let slice (pred : pred) (app1 : Policy.t) (app2 : Policy.t) : Policy.t =
  union ~how:`Parallel
    (seq (filter pred) app1)
    (seq (filter (Neg pred)) app2)

let slice' (pred : Pred.t) (app1 : Policy.t) (app2 : Policy.t) : Policy.t =
  union ~how:`Parallel
    (seq (filter' pred) app1)
    (seq (filter' (neg pred)) app2)
