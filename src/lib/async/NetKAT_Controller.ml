open Core
open Async

open Frenetic_netkat.Syntax
open Frenetic_kernel.OpenFlow

module type PLUGIN = sig
  val start: int -> unit
  val events : event Pipe.Reader.t
  val switch_features : switchId -> switchFeatures option Deferred.t
  val update : Frenetic_netkat.Local_compiler.t -> unit Deferred.t
  val update_switch : switchId -> Frenetic_netkat.Local_compiler.t -> unit Deferred.t
  val packet_out : switchId -> portId option -> payload -> Frenetic_netkat.Syntax.policy list -> unit Deferred.t
  val flow_stats : switchId -> Pattern.t -> flowStats Deferred.t
  val port_stats : switchId -> portId -> portStats Deferred.t
end

module type CONTROLLER = sig
  val start : int -> unit
  val event : unit -> event Deferred.t
  val switches : unit -> (switchId * portId list) list Deferred.t
  val port_stats : switchId -> portId -> portStats Deferred.t
  val update : Frenetic_netkat.Syntax.policy -> unit Deferred.t
  val update_global : Frenetic_netkat.Syntax.policy -> unit Deferred.t
  val update_fdd : Frenetic_netkat.Local_compiler.t -> unit Deferred.t
  val packet_out : switchId -> portId option -> payload -> Frenetic_netkat.Syntax.policy list -> unit Deferred.t
  val query : string -> (int64 * int64) Deferred.t
  val set_current_compiler_options : Frenetic_netkat.Local_compiler.compiler_options -> unit
end

module Make (P:PLUGIN) : CONTROLLER = struct
  (* Global variables *)
  let (pol_reader, pol_writer) = Pipe.create ()
  let (event_reader, event_writer) =  Pipe.create ()
  let switch_hash : (switchId, portId list) Hashtbl.Poly.t = Hashtbl.Poly.create ()
  let current_compiler_options = ref (Frenetic_netkat.Local_compiler.default_compiler_options)
  let fdd = ref (Frenetic_netkat.Local_compiler.compile Frenetic_netkat.Syntax.drop)

  let update (pol:policy) : unit Deferred.t =
    fdd := Frenetic_netkat.Local_compiler.compile ~options:(!current_compiler_options) pol;
    P.update !fdd

  let update_global (pol:policy) : unit Deferred.t =
    fdd := Frenetic_netkat.Global_compiler.compile ~options:(!current_compiler_options) pol;
    P.update !fdd

  let update_fdd new_fdd : unit Deferred.t =
    fdd := new_fdd;
    P.update !fdd

  let handle_event (evt:event) : unit Deferred.t =
    Pipe.write_without_pushback event_writer evt;
    match evt with
    | SwitchUp (sw,ports) ->
       let _ = Hashtbl.Poly.add switch_hash sw ports in
       P.update_switch sw !fdd
    | SwitchDown sw ->
       Hashtbl.Poly.remove switch_hash sw;
       return ()
    | _ ->
       Deferred.return ()

  let start (openflow_port:int) : unit =
    P.start openflow_port;
    don't_wait_for (Pipe.iter P.events ~f:handle_event)

  let event () : event Deferred.t =
    Pipe.read event_reader >>= function
    | `Ok evt -> Deferred.return evt
    | `Eof -> assert false

  let switches () : (switchId * portId list) list Deferred.t =
    return (Hashtbl.Poly.to_alist switch_hash)

  let port_stats (sw : switchId) (pt : portId) : portStats Deferred.t =
    P.port_stats sw pt

  let packet_out (sw:switchId) (ingress_port:portId option) (pay:payload) (pol:policy list) : unit Deferred.t =
    P.packet_out sw ingress_port pay pol

  let get_table (sw_id : switchId) : (Frenetic_kernel.OpenFlow.flow * string list) list =
    Frenetic_netkat.Local_compiler.to_table' sw_id !fdd

  let sum_stat_pairs stats =
     List.fold stats ~init:(0L, 0L)
      ~f:(fun (pkts, bytes) (pkts', bytes') ->
        Int64.(pkts + pkts', bytes + bytes'))

  (* TODO: The NetKAT Controller used to preserve statistics across queries, and
     add the accumulated stats in here.  This is no longer done - is that right? *)

  let query (name : string) : (Int64.t * Int64.t) Deferred.t =
    Deferred.List.map ~how:`Parallel
      (Hashtbl.Poly.keys switch_hash)
      ~f:(fun sw_id ->
        let pats = List.filter_map (get_table sw_id) ~f:(fun (flow, names) ->
          if List.mem names name ~equal:String.equal then
            Some flow.pattern
          else
            None) in
        Deferred.List.map ~how:`Parallel
          pats
          ~f:(fun pat ->
            P.flow_stats sw_id pat
            >>| fun(stats) -> (stats.flow_packet_count, stats.flow_byte_count)
          )
        >>| fun stats -> sum_stat_pairs stats
      )
     >>| fun stats -> sum_stat_pairs stats

  let set_current_compiler_options opt =
    current_compiler_options := opt

end


