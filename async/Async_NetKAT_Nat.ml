open Core.Std
open Async.Std

open Async_NetKAT
open NetKAT_Types


module Log = Async_OpenFlow.Log
let tags = [("netkat", "nat")]

module SwitchMap = Map.Make(Int64)
module MacMap = Map.Make(Int64)

type tpPort = Packet.tpPort
type nwAddr = Packet.nwAddr

module type TABLE = sig
  type t

  (** [create min_port max_port] bounds the range of ports*)
  val create : tpPort -> tpPort -> t

   (** [fresh_public_port private_ip private_port = private_port] where
       [private_port is unused] *)
  val fresh_public_port : t -> nwAddr -> tpPort -> tpPort

  val get_public_port : t -> nwAddr -> tpPort -> tpPort option

  (** [get_private_address public_port = (private_ip, private_port)] *)
  val get_private_addr : t -> tpPort -> (nwAddr * tpPort) option

end


module Table : TABLE = struct
  module H = Hashtbl.Poly 
  type t = {
    min_port : tpPort;
    max_port : tpPort;
    map : (tpPort, nwAddr * tpPort) Hashtbl.t;
    map_rev : (nwAddr * tpPort, tpPort) Hashtbl.t;
  }

  let create min max =
    assert (min < max);
    {
      min_port = min;
      max_port = max;
      map = H.create ();
      map_rev = H.create ()
    }

  let fresh_public_port tbl private_ip private_port = 
    let rec loop public_port = 
      if public_port < tbl.min_port then 
        failwith "NAT is out of ports"
      else if H.mem tbl.map public_port then
        (* Assumes max_port <= 65535 and 65535 + 1 fits in tpPort (int) *)
        loop ((public_port + 1) mod (tbl.max_port + 1))
      else
        begin
          ignore (H.add tbl.map public_port (private_ip, private_port));
          ignore (H.add tbl.map_rev (private_ip, private_port) public_port);
          public_port
        end in
    loop tbl.min_port

  let get_public_port tbl private_ip private_port = 
    H.find tbl.map_rev (private_ip, private_port)

  let get_private_addr tbl public_port = 
    H.find tbl.map public_port

end

let ite pr pol1 pol2 = 
    Union(Seq(Filter pr, pol1),
          Seq(Filter (Neg pr), pol2))

let create public_ip private_phys_port public_phys_port = 
  Printf.eprintf "NAT running\n%!";
  let open Packet in 
  let tbl = Table.create 5000 65535 in
  let public_pol = ref drop in 
  let private_pol = ref (Mod(Location(Pipe "nat"))) in
 
   let mk_pol () = 
     Union(Seq(Filter(Test(Location(Physical(private_phys_port)))), !private_pol), 
           Seq(Filter(Test(Location(Physical(public_phys_port)))), !public_pol)) in 

  let handle_outgoing_packet pk = 
    match pk with
      | { nw = Ip {
            Ip.src = private_ip;
            Ip.tp = Ip.Tcp { Tcp.src = private_port }
          }
        } ->
        begin match Table.get_public_port tbl private_ip private_port with
          | Some public_port -> 
            None
          | None ->
            let public_port =
              Table.fresh_public_port tbl private_ip private_port in
            private_pol :=
              ite (And(Test(IP4Src(private_ip,32l)),
                       Test(TCPSrcPort(private_port))))
                  (Seq(Seq(Mod(TCPSrcPort(public_port)),
                           Mod(IP4Src(public_ip,32l))),
                       Mod(Location(Physical(public_phys_port)))))
               !private_pol;
            public_pol :=
              ite (And(Test(IP4Src(public_ip,32l)),
                       Test(TCPSrcPort(public_port))))
                  (Seq(Seq(Mod(TCPSrcPort(private_port)),
                           Mod(IP4Src(private_ip,32l))),
                       Mod(Location(Physical(private_phys_port)))))
               !public_pol;
            Some (mk_pol ())
        end
      | _ -> 
        None in

  let handler t w () e = match e with
    | SwitchUp(switch_id) ->
      return None
    | SwitchDown(switch_id) ->
      return None
    | PacketIn(_, switch_id, port_id, payload, _) ->
      let packet = Packet.parse (SDN_Types.payload_bytes payload) in
      let pol = handle_outgoing_packet packet in
      let action = SDN_Types.(Output(Physical(public_phys_port))) in
      Pipe.write w (switch_id, (payload, Some(port_id), [action])) >>= fun _ ->
      return pol 
    | _ -> 
      return None in
      
  create ~pipes:(PipeSet.singleton "nat") (mk_pol ()) handler
