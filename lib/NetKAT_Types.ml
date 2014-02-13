open Sexplib.Conv

(** NetKAT Syntax *)

(** {2 Basics} *)

type int8 = SDN_Types.int8
type int16 = SDN_Types.int16
type int32 = SDN_Types.int32
type int48 = SDN_Types.int48
type switchId = SDN_Types.switchId
type portId = SDN_Types.portId
type payload = SDN_Types.payload

(** {2 Policies} *)
  
type location = 
  | Physical of int16 
  | Pipe of string 

type header_val = 
  | Switch of switchId
  | Location of location
  | EthSrc of int48
  | EthDst of int48
  | Vlan of int16
  | VlanPcp of int8
  | EthType of int16
  | IPProto of int8
  | IP4Src of int32
  | IP4Dst of int32
  | TCPSrcPort of int16
  | TCPDstPort of int16

type pred = 
  | True
  | False
  | Test of header_val
  | And of pred * pred
  | Or of pred * pred
  | Neg of pred
      
type policy =
  | Filter of pred
  | Mod of header_val
  | Union of policy * policy
  | Seq of policy * policy
  | Star of policy
  | Link of SDN_Types.switchId * portId * SDN_Types.switchId * portId
      
let id = Filter True
  
let drop = Filter False

(** {2 Packets} 

  If we only defined the semantics and were not building a system, a
  packet would only be a record of headers. However, the runtime needs to
  apply [eval] to packets contained in [PACKET_IN] mesages. For the runtime,
  packets also carry a payload that is unmodified by [eval]. *)

(** A map keyed by header names. *)
module Headers = struct
  (*
   * Note that this module uses the fieldslib library from Jane Street, a syntax
   * extension that generates several convenience functions, .e.g, fold, map,
   * iter, getters, setters, etc., specialized for the record type.  More
   * documentation can be found here:
   *
   *   https://github.com/janestreet/fieldslib
   * *)
  type t = 
      { location : location option sexp_opaque;
        ethSrc : int48 option sexp_opaque;
        ethDst : int48 option sexp_opaque;
        vlan : int16 option sexp_opaque;
        vlanPcp : int8 option sexp_opaque;
        ethType : int16 option sexp_opaque;
        ipProto : int8 option sexp_opaque;
        ipSrc : int32 option sexp_opaque;
        ipDst : int32 option sexp_opaque;
        tcpSrcPort : int16 option sexp_opaque;
        tcpDstPort : int16 option sexp_opaque
      } with sexp,fields

  let compare = Pervasives.compare
end
 
type packet = {
  switch : switchId;
  headers : Headers.t;
  payload : payload
}

module PacketSet = Set.Make (struct
  type t = packet
      
  (* First compare by headers, then payload. The payload comparison is a
     little questionable. However, this is safe to use in eval, since
     all output packets have the same payload as the input packet. *)
  let compare x y =
    let cmp = Headers.compare x.headers y.headers in
    if cmp <> 0 then
      cmp
    else
      Pervasives.compare x.payload y.payload
end)
  
module PacketSetSet = Set.Make(PacketSet)

