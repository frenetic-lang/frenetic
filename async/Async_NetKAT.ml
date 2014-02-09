open Core.Std
open Async.Std

open NetKAT_Types
open Topology

module PipeSet = Set.Make(struct
  type t = string with sexp
  let compare = Pervasives.compare
end)

exception Sequence_error of PipeSet.t * PipeSet.t

type handler = Topology.t -> event -> result Deferred.t

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
  create pol (fun _ _ -> return ([], None))

let create_from_file (filename : string) : app =
  let pol = In_channel.with_file filename ~f:(fun chan ->
    NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_channel chan)) in
  create_static pol

let run (a : app) (t : Topology.t) (e : event) : result Deferred.t =
  match e with
    | PacketIn(p, _, _, _, _, _) when not (PipeSet.mem a.pipes p) ->
      return ([], None)
    | _ ->
      a.handler t e >>| fun (packet_outs, m_pol) ->
        begin match m_pol with
          | Some(pol) -> a.default <- pol
          | None -> ()
        end;
        (packet_outs, m_pol)

let union (a1 : app) (a2 : app) : app =
  { pipes = PipeSet.union a1.pipes a2.pipes
  ; default = Union(a1.default, a2.default)
  ; handler = fun t e ->
      run a1 t e >>= fun (packet_outs1, m_pol1) ->
      run a2 t e >>= fun (packet_outs2, m_pol2) ->
        let packet_outs = packet_outs1 @ packet_outs2 in
        match m_pol1, m_pol2 with
          | None, None ->
            return (packet_outs, None)
          | Some(pol1), Some(pol2) ->
            return (packet_outs, Some(Union(pol1, pol2)))
          | Some(pol1), None ->
            return (packet_outs, Some(Union(pol1, a2.default)))
          | None, Some(pol2) ->
            return (packet_outs, Some(Union(a1.default, pol2)))
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
  ; handler = fun t e ->
      run a1 t e >>= fun (packet_outs1, m_pol1) ->
      run a2 t e >>= fun (packet_outs2, m_pol2) ->
        let packet_outs = packet_outs1 @ packet_outs2 in
        match m_pol1, m_pol2 with
          | None, None ->
            return (packet_outs, None)
          | Some(pol1), Some(pol2) ->
            return (packet_outs, Some(Seq(pol1, pol2)))
          | Some(pol1), None ->
            return (packet_outs, Some(Seq(pol1, a2.default)))
          | None, Some(pol2) ->
            return (packet_outs, Some(Seq(a1.default, pol2)))
  }
