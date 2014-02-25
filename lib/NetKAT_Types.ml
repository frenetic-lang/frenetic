open Sexplib.Conv

(** NetKAT Syntax *)
open Core.Std

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
  | Physical of int32
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
  | IP4Src of int32 * int
  | IP4Dst of int32 * int
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
  | Link of switchId * portId * switchId * portId
      
let id = Filter True
  
let drop = Filter False

(** {2 Packets} 

  If we only defined the semantics and were not building a system, a
  packet would only be a record of headers. However, the runtime needs to
  apply [eval] to packets contained in [PACKET_IN] mesages. For the runtime,
  packets also carry a payload that is unmodified by [eval]. *)

(** A map keyed by header names. *)
module Headers = struct
  open Core.Std
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
        ipSrc : (int32 * int) option sexp_opaque;
        ipDst : (int32 * int) option sexp_opaque;
        tcpSrcPort : int16 option sexp_opaque;
        tcpDstPort : int16 option sexp_opaque
      } with sexp,fields

  let compare = Pervasives.compare

  let empty =
    { location = None;
      ethSrc = None;
      ethDst = None;
      vlan = None;
      vlanPcp = None;
      ethType = None;
      ipProto = None;
      ipSrc = None;
      ipDst = None;
      tcpSrcPort = None;
      tcpDstPort = None }

  let mk_location l = { empty with location = Some l }
  let mk_ethSrc n = { empty with ethSrc = Some n }
  let mk_ethDst n = { empty with ethDst = Some n }
  let mk_vlan n = { empty with vlan = Some n }
  let mk_vlanPcp n = { empty with vlanPcp = Some n }
  let mk_ethType n = { empty with ethType = Some n }
  let mk_ipProto n = { empty with ipProto = Some n }
  let mk_ipSrc (n, m) = { empty with ipSrc = Some (n,m) }
  let mk_ipDst (n, m) = { empty with ipDst = Some (n,m) }
  let mk_tcpSrcPort n = { empty with tcpSrcPort = Some n }
  let mk_tcpDstPort n = { empty with tcpDstPort = Some n }

  let to_string ?init:(init="") ?sep:(sep="=") (x:t) : string =
    let g pp acc f = match Field.get f x with
      | None -> acc
      | Some v ->
        let s = Printf.sprintf "%s%s%s" (Field.name f) sep (pp v) in
        if acc = "" then s
        else Printf.sprintf "%s, %s" acc s in
    let ppl l = match l with
      | Physical x -> Printf.sprintf "%lu" x
      | Pipe x -> x in
    let pp8 = Printf.sprintf "%u" in
    let pp16 = Printf.sprintf "%u" in
    let pp32m (n, m) = 
      match m with 
        | 32 -> Printf.sprintf "%lu" n 
        | _ -> Printf.sprintf "%lu/%d" n m in 
    let pp48 = Printf.sprintf "%Lu" in
    Fields.fold
      ~init:""
      ~location:(g ppl)
      ~ethSrc:(g pp48)
      ~ethDst:(g pp48)
      ~vlan:(g pp16)
      ~vlanPcp:(g pp8)
      ~ethType:(g pp16)
      ~ipProto:(g pp8)
      ~ipSrc:(g pp32m)
      ~ipDst:(g pp32m)
      ~tcpSrcPort:(g pp16)
      ~tcpDstPort:(g pp16)

  let diff (x:t) (y:t) : t =
    let g c acc f =
      match Field.get f x, Field.get f y with
        | Some v1, Some v2 ->
          c f v1 v2 acc
        | _ ->
          acc in
    let c f v1 v2 acc =
      if v1 = v2 then Field.fset f acc None else acc in
    let cm f (v1,m1) (v2,m2) acc =
      let b = max 0 (max (32-m1) (32-m2)) in
      if m2 >= m1 && Int32.shift_right v1 b = Int32.shift_right v2 b then
        Field.fset f acc None
      else
        acc in
    Fields.fold
      ~init:x
      ~location:(g c)
      ~ethSrc:(g c)
      ~ethDst:(g c)
      ~vlan:(g c)
      ~vlanPcp:(g c)
      ~ethType:(g c)
      ~ipProto:(g c)
      ~ipSrc:(g cm)
      ~ipDst:(g cm)
      ~tcpSrcPort:(g c)
      ~tcpDstPort:(g c)
end
 
type packet = {
  switch : switchId;
  headers : Headers.t;
  payload : payload
}

module PacketSet = Set.Make (struct
  type t = packet sexp_opaque with sexp
      
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

(** {3 Applications} *)

type action = SDN_Types.action


type switch_port = switchId * portId
type host = Packet.dlAddr * Packet.nwAddr

type bufferId = Int32.t (* XXX(seliopou): different than SDN_Types *)
type bytes = Packet.bytes

type event =
  | PacketIn of string * switchId * portId * bytes * int * bufferId option
  | Query of string * int64 * int64
  | SwitchUp of switchId
  | SwitchDown of switchId
  | PortUp of switch_port
  | PortDown of switch_port
  | LinkUp of switch_port * switch_port
  | HostUp of switch_port * host
  | HostDown of switch_port * host

type packet_out = switchId * bytes * bufferId option * portId option * action list
