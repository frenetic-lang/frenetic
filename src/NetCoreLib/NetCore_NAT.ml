open Printf
open Packet
open NetCore_Types
open NetCore_Action.Output
open NetCore_Pattern

(** Table relating private locations to public locations. *)
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
      map = Hashtbl.create 100;
      map_rev = Hashtbl.create 100
    }

  let fresh_public_port tbl private_ip private_port = 
    let rec loop public_port = 
      if public_port < tbl.min_port then 
        failwith "NAT is out of ports"
      else if Hashtbl.mem tbl.map public_port then
        (* Assumes max_port <= 65535 and 65535 + 1 fits in tpPort (int) *)
        loop ((public_port + 1) mod (tbl.max_port + 1))
      else
        begin
          Hashtbl.add tbl.map public_port (private_ip, private_port);
          Hashtbl.add tbl.map_rev (private_ip, private_port) public_port;
          public_port
        end in
    loop tbl.min_port

  let get_public_port tbl private_ip private_port = 
    try
      Some (Hashtbl.find tbl.map_rev (private_ip, private_port))
    with Not_found -> None 

  let get_private_addr tbl public_port = 
    try Some (Hashtbl.find tbl.map public_port)
    with Not_found -> None

end

let make (public_ip : nwAddr) =
  let (stream, push) = Lwt_stream.create () in
  let tbl = Table.create 2000 65535 in
  let rec init_public_pol sw pt pk =
    match pk with
      | { dlTyp = 0x800;
          nw = Ip {
            Ip.src = src_ip;
            Ip.dst = dst_ip;
            Ip.proto = 6;
            Ip.tp = Ip.Tcp { Tcp.src = src_pt; Tcp.dst = dst_pt }
          }
        } -> 
        eprintf "[NAT] firewall dropping IP packet from %s:%d to %s:%d\n%!"
          (string_of_ip src_ip) src_pt (string_of_ip dst_ip) dst_pt;
        drop 
      | _ -> eprintf "[NAT] firewalling non IP packet.\n%!"; drop in
                      
  let rec callback sw pt pk =
    match pk with
      | { dlTyp = 0x800;
          nw = Ip {
            Ip.src = private_ip;
            Ip.proto = 6;
            Ip.tp = Ip.Tcp { Tcp.src = private_port }
          }
        } ->
        begin match Table.get_public_port tbl private_ip private_port with
          | Some public_port ->
            seq_action
              (updateSrcIP private_ip public_ip)
              (updateSrcPort private_port public_port)
          | None ->
            let public_port =
              Table.fresh_public_port tbl private_ip private_port in
            Printf.eprintf "[NAT] translating %s:%d to %s:%d\n%!"
              (string_of_ip private_ip) private_port
              (string_of_ip public_ip) public_port;
            private_pol :=
              PoITE
              (PrAnd (PrHdr (ipSrc private_ip),
                      PrHdr (tcpSrcPort private_port)),
               PoSeq (PoAction (updateSrcIP private_ip public_ip),
                      PoAction (updateSrcPort private_port public_port)),
               !private_pol);
            public_pol :=
              PoITE
              (PrAnd (PrHdr (ipDst public_ip),
                      PrHdr (tcpDstPort public_port)),
               PoSeq (PoAction (updateDstIP public_ip private_ip),
                      PoAction (updateDstPort public_port private_port)),
               !public_pol);
            push (Some (!private_pol, !public_pol));
            seq_action
              (updateSrcIP private_ip public_ip)
              (updateSrcPort private_port public_port)
        end
      | _ -> drop
  and private_pol = ref (PoAction [ControllerAction callback])
  and public_pol = ref (PoAction [ControllerAction init_public_pol]) in
  let (lwt_computation, pair_stream) =
    NetCore_Stream.from_stream (!private_pol, !public_pol) stream in
  (lwt_computation,
   NetCore_Stream.map (fun (priv, _) -> priv) pair_stream,
   NetCore_Stream.map (fun (_, pub) -> pub) pair_stream)
