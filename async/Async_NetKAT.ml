open Core.Std
open Async.Std

open NetKAT_Types

exception Assertion_failed of string

type node =
    | Switch of SDN_Types.switchId
    | Host of Packet.dlAddr * Packet.nwAddr

module Node = struct
  type t = node

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
  type t = unit

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

type app = (Net.Topology.t ref) Raw_app.t

type 'phantom pipes = {
  pkt_out : (switchId * SDN_Types.pktOut, 'phantom) Pipe.t;
  update  : (policy, 'phantom) Pipe.t
}

type send = Pipe.Writer.phantom pipes
type recv = Pipe.Reader.phantom pipes

type result = policy option
type handler
  = Net.Topology.t ref
  -> (switchId * SDN_Types.pktOut) Pipe.Writer.t
  -> unit
  -> event -> result Deferred.t

type async_handler
  = Net.Topology.t ref
  -> send
  -> unit
  -> event -> result Deferred.t

let create ?pipes (policy : policy) (handler : handler) : app =
  let open Raw_app in
  create_primitive ?pipes policy (fun a send () ->
    let callback = handler a send.pkt_out () in
    fun e ->
      callback e
      >>= function
        | None    -> Pipe.write send.update EventNoop
        | Some(p) -> Pipe.write send.update (Event p))

let create_async ?pipes (policy : policy) (handler : async_handler) : app =
  let open Raw_app in
  create_primitive ?pipes policy (fun a send () ->
    let r_update, w_update = Pipe.create () in
    Deferred.don't_wait_for
      (transfer_batch r_update send.update ~f:(fun p -> (Async p)));
    let callback = handler a { pkt_out = send.pkt_out; update = w_update } () in
    fun e ->
      callback e
      >>= function
        | None    -> Pipe.write send.update EventNoop
        | Some(p) -> Pipe.write send.update (Event p))

let create_static (policy : policy) : app =
  Raw_app.create_static policy

let create_from_string (str : string) : app =
  let pol = NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_string str) in
  create_static pol

let create_from_file (filename : string) : app =
  let pol = In_channel.with_file filename ~f:(fun chan ->
    NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_channel chan)) in
  create_static pol

let default (a : app) : policy =
  a.Raw_app.policy

let run (app : app) (t : Net.Topology.t ref) () : (recv * (event -> unit Deferred.t)) =
  let recv, callback = Raw_app.run app t () in
  { pkt_out = recv.Raw_app.pkt_out; update = recv.Raw_app.update }, callback

let union ?(how=`Parallel) (app1 : app) (app2 : app) : app =
  Raw_app.combine ~how:how (fun x y -> Union(x, y)) app1 app2

let seq (app1 : app) (app2 : app) : app =
  let open Raw_app in
  begin if not PipeSet.(is_empty (inter app1.pipes app2.pipes)) then
    (* In order for the form of composition below, the apps must not be
     * listening on the same pipe for `PacketIn` events. In this case,
     * only one of the apps will actually run and produce PacketOut messages
     * on a `PacketIn` event. *)
    raise (Sequence_error(app1.pipes, app2.pipes))
  end;
  Raw_app.combine ~how:`Sequential (fun x y -> Seq(x, y)) app1 app2

let guard (pred : pred) (app : app) : app =
  Raw_app.guard pred app

let slice (pred : pred) (app1 : app) (app2 : app) : app =
  union ~how:`Parallel
    (Raw_app.guard pred       app1)
    (Raw_app.guard (Neg pred) app2)
