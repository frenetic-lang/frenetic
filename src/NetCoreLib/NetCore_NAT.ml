open Packet
open NetCore_Types.External

(** Table relating private locations to public locations. *)
module type TABLE = sig
  type t

  (** [create min_port max_port] bounds the range of ports*)
  val create : tpPort -> tpPort -> t

   (** [fresh_public_port private_ip private_port = private_port] where
       [private_port is unused] *)
  val fresh_public_port : t -> nwAddr -> tpPort -> tpPort

  (** [get_private_address public_port = (private_ip, private_port)] *)
  val get_private_addr : t -> tpPort -> nwAddr * tpPort

end


module Table : TABLE = struct

  type t = {
    min_port : tpPort;
    max_port : tpPort;
    map : (tpPort, nwAddr * tpPort) Hashtbl.t
  }

  let create min max =
    assert (min < max);
    {
      min_port = min;
      max_port = max;
      map = Hashtbl.create 100 (* more connections than any home needs, IMHO *)
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
          public_port
        end in
    loop tbl.min_port

  let get_private_addr tbl public_port = Hashtbl.find tbl.map public_port

end

let make (public_ip : nwAddr) =
  let (stream, push) = Lwt_stream.create () in
  let tbl = Table.create 2000 65535 in
  let rec callback sw pt pk =
    match pk with
      | { pktDlTyp = 0x800;
          pktNwHeader = NwIP {
            pktIPSrc = private_ip;
            pktIPProto = 6;
            pktTpHeader = TpTCP { tcpSrc = private_port }
          }
        } ->
        let public_port = Table.fresh_public_port tbl private_ip private_port in
        private_pol :=
          Seq (Seq (Act (UpdateSrcIP (private_ip, public_ip)),
                    Act (UpdateSrcPort (private_port, public_port))),
               !private_pol);
        public_pol :=
          Seq (Seq (Act (UpdateSrcIP (public_ip, private_ip)),
                    Act (UpdateSrcPort (public_port, private_port))),
               !public_pol);
        push (Some (!private_pol, !public_pol))
      | _ -> ()
  and private_pol = ref (Act (GetPacket callback))
  and public_pol = ref Empty in
  NetCore_Stream.from_stream (!private_pol, !public_pol) stream
