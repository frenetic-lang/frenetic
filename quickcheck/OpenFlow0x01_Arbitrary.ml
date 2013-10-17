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

module type OpenFlow0x01_ArbitraryCstruct = sig
  type t

  val arbitrary : t arbitrary

  val to_string : t -> string

  val parse : Cstruct.t -> t
  val marshal : t -> Cstruct.t -> int

  val size_of : t -> int

end

module OpenFlow0x01_Unsize(ArbC : OpenFlow0x01_ArbitraryCstruct) = struct
  type t = ArbC.t
  type s = Cstruct.t

  let arbitrary = ArbC.arbitrary

  let to_string = ArbC.to_string

  let parse = ArbC.parse

  let marshal m =
    let bytes = Cstruct.of_bigarray Bigarray.(Array1.create char c_layout (ArbC.size_of m))
      in ignore (ArbC.marshal m bytes); bytes
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

  let arbitrary_dlAddr = arbitrary_uint48
  let arbitrary_dlVlan = arbitrary_option arbitrary_uint16
  let arbitrary_dlVlanPcp = arbitrary_uint8
  let arbitrary_nwAddr = arbitrary_int32
  let arbitrary_nwTos = arbitrary_uint8
  let arbitrary_tpPort = arbitrary_uint16

  let arbitrary =
    let open Gen in
    let open OpenFlow0x01_Core in
    arbitrary_option arbitrary_dlAddr >>= fun dlSrc ->
    arbitrary_option arbitrary_dlAddr >>= fun dlDst ->
    arbitrary_option arbitrary_uint16 >>= fun dlTyp ->
    arbitrary_option arbitrary_dlVlan >>= fun dlVlan ->
    arbitrary_option arbitrary_dlVlanPcp >>= fun dlVlanPcp ->
    arbitrary_option arbitrary_nwAddr >>= fun nwSrc ->
    arbitrary_option arbitrary_nwAddr >>= fun nwDst ->
    arbitrary_option arbitrary_uint8 >>= fun nwProto ->
    arbitrary_option arbitrary_nwTos >>= fun nwTos ->
    arbitrary_option arbitrary_tpPort >>= fun tpSrc ->
    arbitrary_option arbitrary_tpPort >>= fun tpDst ->
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
  let marshal = Match.marshal

  let size_of = Match.size_of

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
        arbitrary_uint >>= (fun l -> ret_gen (Controller l))
      ]

  (* Use in cases where a `Controller` port is invalid input *)
  let arbitrary_nc =
    let open Gen in
    let open OpenFlow0x01_Core in
      oneof [
        arbitrary_uint16 >>= (fun p -> ret_gen (PhysicalPort p));
        ret_gen AllPorts;
        ret_gen InPort;
        ret_gen Flood;
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

module Action = struct
  type t = Action.t
  type s = Cstruct.t

  let arbitrary =
    let open Gen in
    let open OpenFlow0x01_Core in
    oneof [
      PseudoPort.arbitrary >>= (fun p -> ret_gen (Output p));
      Match.arbitrary_dlVlan >>= (fun dlVal -> ret_gen (SetDlVlan dlVal));
      Match.arbitrary_dlVlanPcp >>= (fun dlValPcp -> ret_gen (SetDlVlanPcp dlValPcp));
      Match.arbitrary_dlAddr >>= (fun dlSrc -> ret_gen (SetDlSrc dlSrc));
      Match.arbitrary_dlAddr >>= (fun dlDst -> ret_gen (SetDlDst dlDst));
      Match.arbitrary_nwAddr >>= (fun nwSrc -> ret_gen (SetNwSrc nwSrc));
      Match.arbitrary_nwAddr >>= (fun nwDst -> ret_gen (SetNwDst nwDst));
      Match.arbitrary_nwTos >>= (fun nwTos -> ret_gen (SetNwTos nwTos));
      Match.arbitrary_tpPort >>= (fun tpSrc -> ret_gen (SetTpSrc tpSrc));
      Match.arbitrary_tpPort >>= (fun tpDst -> ret_gen (SetTpDst tpDst));
      PseudoPort.arbitrary_nc >>= (fun p ->
          arbitrary_int32 >>= (fun g -> ret_gen (Enqueue (p, g))))
    ]

  let to_string = Action.to_string

  let marshal = Action.marshal
  let parse = Action.parse

  let size_of = Action.size_of

end

module FlowMod = struct

  module Command = struct
    type t = FlowMod.Command.t
    type s = Packet.int16

    let arbitrary =
      let open Gen in
      let open OpenFlow0x01_Core in
      oneof [
        ret_gen AddFlow;
        ret_gen ModFlow;
        ret_gen ModStrictFlow;
        ret_gen DeleteFlow;
        ret_gen DeleteStrictFlow
      ]

    let to_string = FlowMod.Command.to_string

    let marshal = FlowMod.Command.to_int
    let parse = FlowMod.Command.of_int
  end

  module Timeout = struct
    type t = FlowMod.Timeout.t
    type s = Packet.int16

    let arbitrary =
      let open Gen in
      let open OpenFlow0x01_Core in
      oneof [
        ret_gen Permanent;
        arbitrary_uint16 >>= (fun n -> ret_gen (ExpiresAfter n))
      ]

    let to_string = FlowMod.Timeout.to_string

    let marshal = FlowMod.Timeout.to_int
    let parse = FlowMod.Timeout.of_int
  end

  type t = FlowMod.t
  type s = Cstruct.t

  let to_string = FlowMod.to_string

  let arbitrary =
    let open Gen in
    let open OpenFlow0x01_Core in
      Command.arbitrary >>= fun command ->
      Match.arbitrary >>= fun pattern ->
      arbitrary_uint16 >>= fun priority ->
      arbitrary_list Action.arbitrary >>= fun actions ->
      arbitrary_uint48 >>= fun cookie ->
      Timeout.arbitrary >>= fun idle_timeout ->
      Timeout.arbitrary >>= fun hard_timeout ->
      arbitrary_bool >>= fun notify_when_removed ->
      arbitrary_option arbitrary_uint32 >>= fun apply_to_packet ->
      arbitrary_option PseudoPort.arbitrary_nc >>= fun out_port ->
      arbitrary_bool >>= fun check_overlap ->
        ret_gen {
          command = command;
          pattern = pattern;
          priority = priority;
          actions = OpenFlow0x01.Action.move_controller_last actions;
          cookie = cookie;
          idle_timeout = idle_timeout;
          hard_timeout = hard_timeout;
          notify_when_removed = notify_when_removed;
          apply_to_packet = apply_to_packet;
          out_port = out_port;
          check_overlap = check_overlap
        }

  let marshal = FlowMod.marshal
  let parse = FlowMod.parse

  let size_of = FlowMod.size_of

end
