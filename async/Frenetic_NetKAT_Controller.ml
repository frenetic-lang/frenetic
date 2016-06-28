open Core.Std
open Async.Std

open Frenetic_NetKAT
open Frenetic_OpenFlow

module Log = Frenetic_Log

module type PLUGIN = sig
  val start: int -> unit
  val events : event Pipe.Reader.t
  val switch_features : switchId -> switchFeatures option Deferred.t
  val update : Frenetic_NetKAT_Compiler.t -> unit Deferred.t
  val update_switch : switchId -> Frenetic_NetKAT_Compiler.t -> unit Deferred.t
  val packet_out : switchId -> payload -> Frenetic_NetKAT_Compiler.t -> unit Deferred.t
  val flow_stats : switchId -> Frenetic_NetKAT.pred -> flowStats Deferred.t
  val port_stats : switchId -> portId -> portStats Deferred.t
end

module type CONTROLLER = sig
  val start : int -> unit
  val event : unit -> event Deferred.t
  val switches : unit -> (switchId * portId list) list Deferred.t
  val port_stats : switchId -> portId -> portStats Deferred.t
  val update : Frenetic_NetKAT.policy -> unit Deferred.t
  val packet_out : switchId -> payload -> Frenetic_NetKAT.policy -> unit Deferred.t
  val query : string -> (int64 * int64) Deferred.t
end
                                     
module Make (P:PLUGIN) : CONTROLLER = struct
  (* Global variables *)
  let (pol_reader, pol_writer) = Pipe.create ()
  let (event_reader, event_writer) =  Pipe.create ()
  let switch_hash : (switchId, portId list) Hashtbl.Poly.t = Hashtbl.Poly.create ()
  let fdd = ref (Frenetic_NetKAT_Compiler.compile_local Frenetic_NetKAT.drop) 
                                                                               
  let update (pol:policy) : unit Deferred.t =
    fdd := Frenetic_NetKAT_Compiler.compile_local pol;    
    P.update !fdd

  let handle_event (evt:event) : unit Deferred.t =
    Pipe.write_without_pushback event_writer evt;
    match evt with
    | SwitchUp (sw,ports) -> 
       (* TODO: This used to be add_exn, but it was blowing up.  Just ignore return value now. *)
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

  let packet_out (sw:switchId) (pay:payload) (pol:policy) : unit Deferred.t =
    P.packet_out sw pay (Frenetic_NetKAT_Compiler.compile_local pol)
                 
  let query (q:string) : (int64 * int64) Deferred.t =
    let _,pred = List.find_exn (Frenetic_NetKAT_Compiler.queries !fdd) ~f:(fun (q',_) -> q' = q) in 
    Hashtbl.Poly.fold
      switch_hash ~init:(return (0L, 0L))
      ~f:(fun ~key:sw ~data:_ d ->
          d >>= fun (packets,bytes) -> 
          P.flow_stats sw pred >>= fun fs ->
          return (Int64.(packets + fs.flow_packet_count), Int64.(bytes + fs.flow_byte_count)))
end


