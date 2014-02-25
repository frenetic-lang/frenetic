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
end

module Link = struct
  type t = unit

  let compare = Pervasives.compare

  let to_string () = "()"
  let default = ()

  let parse_dot _ = failwith "NYI: Link.parse_dot"
  let parse_gml _ = failwith "NYI: Link.parse_dot"
end

module Net = Network.Make(Node)(Link)

module PipeSet = Set.Make(struct
  type t = string with sexp
  let compare = Pervasives.compare
end)

exception Sequence_error of PipeSet.t * PipeSet.t

type result = policy option
type handler = Net.Topology.t ref -> packet_out Pipe.Writer.t -> event -> result Deferred.t

type app = {
  pipes : PipeSet.t;
  handler : handler;
  mutable default : policy
}

let create ?pipes (default : policy) (handler : handler) : app =
  let pipes = match pipes with
    | None -> PipeSet.empty
    | Some(pipes) -> pipes in
  { pipes; default; handler }

let create_static (pol : policy) : app =
  create pol (fun _ _ _ -> return None)

let create_from_file (filename : string) : app =
  let pol = In_channel.with_file filename ~f:(fun chan ->
    NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_channel chan)) in
  create_static pol

let default (a : app) : policy =
  a.default

let run
    (a : app)
    (t : Net.Topology.t ref)
    (w : packet_out Pipe.Writer.t)
    (e : event)
    : result Deferred.t =
  match e with
    | PacketIn(p, _, _, _, _, _) when not (PipeSet.mem a.pipes p) ->
      return None
    | _ ->
      a.handler t w e >>| fun m_pol ->
        begin match m_pol with
          | Some(pol) -> a.default <- pol
          | None -> ()
        end;
        m_pol

let union ?(how=`Parallel) (a1 : app) (a2 : app) : app =
  { pipes = PipeSet.union a1.pipes a2.pipes
  ; default = Union(a1.default, a2.default)
  ; handler = fun t w e ->
      Deferred.List.map ~how:how ~f:(fun a -> run a t w e) [a1; a2]
      >>= function
        | [m_pol1; m_pol2] ->
          begin match m_pol1, m_pol2 with
            | None, None ->
              return None
            | Some(pol1), Some(pol2) ->
              return (Some(Union(pol1, pol2)))
            | Some(pol1), None ->
              return (Some(Union(pol1, a2.default)))
            | None, Some(pol2) ->
              return (Some(Union(a1.default, pol2)))
          end
        | _ -> raise (Assertion_failed "Async_NetKAT.union: impossible length list")
  }

let seq (a1 : app) (a2: app) : app =
  begin if not PipeSet.(is_empty (inter a1.pipes a2.pipes)) then
    (* In order for the form of composition below, the apps must not be
     * listening on the same pipe for `PacketIn` events. In this case,
     * only one of the apps will actually run and produce PacketOut messages
     * on a `PacketIn` event. *)
    raise (Sequence_error(a1.pipes, a2.pipes))
  end;
  { pipes = PipeSet.union a1.pipes a2.pipes
  ; default = Seq(a1.default, a2.default)
  ; handler = fun t w e ->
      run a1 t w e >>= fun m_pol1 ->
      run a2 t w e >>= fun m_pol2 ->
        match m_pol1, m_pol2 with
          | None, None ->
            return None
          | Some(pol1), Some(pol2) ->
            return (Some(Seq(pol1, pol2)))
          | Some(pol1), None ->
            return (Some(Seq(pol1, a2.default)))
          | None, Some(pol2) ->
            return (Some(Seq(a1.default, pol2)))
  }
