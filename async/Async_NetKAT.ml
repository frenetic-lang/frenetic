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
end

module Link = struct
  type t = unit

  let compare = Pervasives.compare

  let to_string () = "()"
  let default = ()

  let parse_dot _ = failwith "NYI: Link.parse_dot"
  let parse_gml _ = failwith "NYI: Link.parse_dot"

  let to_dot _ = failwith "NYI: Link.to_dot"
end

module Net = Network.Make(Node)(Link)
module PipeSet = Raw_app.PipeSet

exception Sequence_error of PipeSet.t * PipeSet.t

type app = (Net.Topology.t ref) Raw_app.t

type update = Raw_app.update

type send = update Raw_app.send
type recv = policy Raw_app.recv

type result = policy option
type handler
  = Net.Topology.t ref
  -> (switchId * SDN_Types.pktOut) Pipe.Writer.t
  -> unit
  -> event -> result Deferred.t

type async_handler
  = Net.Topology.t ref
  -> policy Raw_app.send
  -> unit
  -> event -> result Deferred.t

let create ?pipes (policy : policy) (handler : handler) : app =
  let open Raw_app in
  create_primitive ?pipes policy (fun a send () ->
    let callback = handler a send.pkt_out () in
    fun e ->
      callback e
      >>= function
        | None    -> Pipe.write send.update Raw_app.EventNoop
        | Some(p) -> Pipe.write send.update (Raw_app.Event p))

let create_async ?pipes (policy : policy) (handler : async_handler) : app =
  let open Raw_app in
  create_primitive ?pipes policy (fun a send () ->
    let r_update, w_update = Pipe.create () in
    Deferred.don't_wait_for
      (Pipe.transfer r_update send.update ~f:(fun p -> Raw_app.Async(p)));
    let callback = handler a { pkt_out = send.pkt_out; update = w_update } () in
    fun e ->
      callback e
      >>= function
        | None    -> Pipe.write send.update Raw_app.EventNoop
        | Some(p) -> Pipe.write send.update (Raw_app.Event p))

let create_static (pol : policy) : app =
  create pol (fun _ _ () _ -> return None)

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
  Raw_app.run app t ()

let union ?(how=`Parallel) (app1 : app) (app2 : app) : app =
  Raw_app.combine ~how:how (fun x y -> Union(x, y)) app1 app2

let seq app1 app2 = (* (app1 : app) (app2: app) : app = *)
  let open Raw_app in
  begin if not PipeSet.(is_empty (inter app1.pipes app2.pipes)) then
    (* In order for the form of composition below, the apps must not be
     * listening on the same pipe for `PacketIn` events. In this case,
     * only one of the apps will actually run and produce PacketOut messages
     * on a `PacketIn` event. *)
    raise (Sequence_error(app1.pipes, app2.pipes))
  end;
  Raw_app.combine ~how:`Sequential (fun x y -> Seq(x, y)) app1 app2
