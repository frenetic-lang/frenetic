open OpenFlow0x01

open QuickCheck
module Gen = QuickCheck_gen

(* arbitrary instance for usigned integers, using `int` type. *)
let arbitrary_uint = Gen.sized (fun n -> Gen.choose_int (0, n))

(* arbitrary instance for unsigned int8, using the `int` type. *)
let arbitrary_uint8 = Gen.choose_int (0, 255)

(* arbitrary instance for unsigned int16, using the `int` type. *)
let arbitrary_uint16 =
  let open Gen in
  arbitrary_uint8 >>= fun a ->
  arbitrary_uint8 >>= fun b ->
    ret_gen (a + b)

(* arbitrary instance for unsigned int32, using the `int32` type. *)
let arbitrary_uint32 =
  let open Gen in
  arbitrary_uint16 >>= fun a ->
  arbitrary_uint16 >>= fun b ->
    ret_gen Int32.(add (of_int a) (of_int b))

(* arbitrary instance for unsigned int48, using the `int64` type. *)
let arbitrary_uint48 =
  let open Gen in
  arbitrary_uint32 >>= fun a ->
  arbitrary_uint16 >>= fun b ->
    ret_gen Int64.(add (of_int32 a) (of_int b))

(* arbitrary instance for option type, favoring `Some` rather than `None` *)
let arbitrary_option arb =
  let open Gen in
  frequency [
      (1, ret_gen None);
      (3, arb >>= fun e -> ret_gen (Some e)) ]

module type OpenFlow0x01_Arbitrary = sig
    type t
    type s

    val arbitrary : t arbitrary

    val to_string : t -> string

    val parse : s -> t
    val marshal : t -> s
end

module Wildcards = struct
  type t = Wildcards.t
  type s = Int32.t

  let arbitrary : t arbitrary =
    let open Gen in
    let open Wildcards in
    arbitrary_bool >>= fun in_port ->
    arbitrary_bool >>= fun dl_vlan ->
    arbitrary_bool >>= fun dl_src ->
    arbitrary_bool >>= fun dl_dst ->
    arbitrary_bool >>= fun dl_type ->
    arbitrary_bool >>= fun nw_proto ->
    arbitrary_bool >>= fun tp_src ->
    arbitrary_bool >>= fun tp_dst ->
    arbitrary_uint >>= fun nw_src ->
    arbitrary_uint >>= fun nw_dst ->
    arbitrary_bool >>= fun dl_vlan_pcp ->
    arbitrary_bool >>= fun nw_tos ->
        ret_gen {
            in_port = in_port;
            dl_vlan = dl_vlan;
            dl_src = dl_src;
            dl_dst = dl_dst;
            dl_type = dl_type;
            nw_proto = nw_proto;
            tp_src = tp_src;
            tp_dst = tp_dst;
            nw_src = nw_src;
            nw_dst = nw_dst;
            dl_vlan_pcp = dl_vlan_pcp;
            nw_tos = nw_tos
          }

    let to_string = Wildcards.to_string

    let parse = Wildcards.parse
    let marshal = Wildcards.marshal
end

module Match = struct
  type t = Match.t
  type s = Cstruct.t

  let arbitrary_dlVlan = arbitrary_option arbitrary_uint16

  let arbitrary =
    let open Gen in
    let open OpenFlow0x01_Core in
    arbitrary_option arbitrary_uint48 >>= fun dlSrc ->
    arbitrary_option arbitrary_uint48 >>= fun dlDst ->
    arbitrary_option arbitrary_uint16 >>= fun dlTyp ->
    arbitrary_option arbitrary_dlVlan >>= fun dlVlan ->
    arbitrary_option arbitrary_uint8 >>= fun dlVlanPcp ->
    arbitrary_option arbitrary_uint32 >>= fun nwSrc ->
    arbitrary_option arbitrary_uint32 >>= fun nwDst ->
    arbitrary_option arbitrary_uint8 >>= fun nwProto ->
    arbitrary_option arbitrary_uint8 >>= fun nwTos ->
    arbitrary_option arbitrary_uint16 >>= fun tpSrc ->
    arbitrary_option arbitrary_uint16 >>= fun tpDst ->
    arbitrary_option arbitrary_uint16 >>= fun inPort ->
        ret_gen {
          dlSrc = dlSrc;
          dlDst = dlDst;
          dlTyp = dlTyp;
          dlVlan = dlVlan;
          dlVlanPcp = dlVlanPcp;
          nwSrc = nwSrc;
          nwDst = nwDst;
          nwProto = nwProto;
          nwTos = nwTos;
          tpSrc = tpSrc;
          tpDst = tpDst;
          inPort= inPort
        }

  let to_string = Match.to_string

  let parse = Match.parse

  let marshal m =
    let bytes = Cstruct.of_bigarray Bigarray.(Array1.create char c_layout 50)
      in ignore (Match.marshal m bytes); bytes
end

module PseudoPort = struct
  (* 
   * Keep track of the `Controller` constructor parameter in the serialization
   * type, if it exists. The `marshal` function is a lossy transformation and
   * drops this piece of data. Manually preserve it to make roundtrip tests
   * work.
   *)
  type s = int * (int option)
  type t = PseudoPort.t

  let arbitrary =
    let open Gen in
    let open OpenFlow0x01_Core in
      oneof [
        arbitrary_uint16 >>= (fun p -> ret_gen (PhysicalPort p));
        ret_gen AllPorts;
        ret_gen InPort;
        ret_gen Flood;
        arbitrary_int >>= (fun l -> ret_gen (Controller l))
      ]

  let to_string = PseudoPort.to_string

  let parse (p, l) = 
    let l' = match l with
             | None   -> 0
             | Some i -> i
      in PseudoPort.make p l'

  let marshal p = 
    let open OpenFlow0x01_Core in
    let l = match p with
            | Controller i -> Some i
            | _            -> None 
      in (PseudoPort.marshal p, l)
end
