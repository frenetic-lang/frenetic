open OpenFlow0x01
open Arbitrary_Base

open QuickCheck
module Gen = QuickCheck_gen


let arbitrary_32mask =
  let open Gen in
  (choose_int (1, 32)) >>= fun a ->
    ret_gen (Int32.of_int a)

let arbitrary_masked arb arb_mask =
  let open OpenFlow0x01_Core in
  let open Gen in
  frequency [
    (1, arb >>= fun v -> ret_gen {OpenFlow0x01_Core.m_value = v; m_mask = None});
    (3, arb >>= fun v ->
        arb_mask >>= fun m -> ret_gen {OpenFlow0x01_Core.m_value = v; m_mask = Some m}) ]

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
    arbitrary_option (arbitrary_masked arbitrary_nwAddr arbitrary_32mask) >>= fun nwSrc ->
    arbitrary_option (arbitrary_masked arbitrary_nwAddr arbitrary_32mask) >>= fun nwDst ->
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
        choose_int(0,0xff00) >>= (fun p -> ret_gen (PhysicalPort p));
        ret_gen InPort;
        (* ret_gen Table; *)
        ret_gen Normal;
        ret_gen Flood;
        ret_gen AllPorts;
        arbitrary_uint >>= (fun l -> ret_gen (Controller l));
        ret_gen Local
      ]

  (* Use in cases where a `Controller` port is invalid input *)
  let arbitrary_nc =
    let open Gen in
    let open OpenFlow0x01_Core in
      oneof [
        arbitrary_uint16 >>= (fun p -> ret_gen (PhysicalPort p));
        ret_gen InPort;
        (* ret_gen Table; *)
        ret_gen Normal;
        ret_gen Flood;
        ret_gen AllPorts;
        ret_gen Local
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

module Timeout = struct
  type t = OpenFlow0x01.Timeout.t
  type s = Packet.int16

  let arbitrary =
    let open Gen in
    let open OpenFlow0x01_Core in
    oneof [
      ret_gen Permanent;
      arbitrary_uint16 >>= (fun n -> ret_gen (ExpiresAfter n))
    ]

  let to_string = OpenFlow0x01.Timeout.to_string

  let marshal = OpenFlow0x01.Timeout.to_int
  let parse = OpenFlow0x01.Timeout.of_int
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

module FlowRemoved = struct

  module Reason = struct
    type t = FlowRemoved.Reason.t
    type s = Packet.int8

    let arbitrary =
      let open Gen in
      let open OpenFlow0x01_Core in
      oneof [
        ret_gen IdleTimeout;
        ret_gen HardTimeout;
        ret_gen Delete;
      ]

    let to_string = FlowRemoved.Reason.to_string

    let marshal = FlowRemoved.Reason.to_int
    let parse = FlowRemoved.Reason.of_int
  end

  type t = FlowRemoved.t
  type s = Cstruct.t

  let arbitrary =
    let open Gen in
    let open OpenFlow0x01_Core in
      Match.arbitrary >>= fun pattern ->
      arbitrary_uint48 >>= fun cookie ->
      arbitrary_uint16 >>= fun priority ->
      Reason.arbitrary >>= fun reason ->
      arbitrary_uint32 >>= fun duration_sec ->
      arbitrary_uint32 >>= fun duration_nsec ->
      Timeout.arbitrary >>= fun idle_timeout ->
      arbitrary_uint48 >>= fun packet_count ->
      arbitrary_uint48 >>= fun byte_count ->
        ret_gen {
          pattern = pattern;
          cookie = cookie;
          priority = priority;
          reason = reason;
          duration_sec = duration_sec;
          duration_nsec = duration_nsec;
          idle_timeout = idle_timeout;
          packet_count = packet_count;
          byte_count = byte_count
        }

  let to_string = FlowRemoved.to_string

  let marshal = FlowRemoved.marshal
  let parse = FlowRemoved.parse

  let size_of = FlowRemoved.size_of

end

module PortDescription = struct
  module PortConfig = struct
    open PortDescription

    type t = PortConfig.t
    type s = Int32.t

    let arbitrary =
      let open Gen in
      let open PortConfig in
      arbitrary_bool >>= fun down ->
      arbitrary_bool >>= fun no_stp ->
      arbitrary_bool >>= fun no_recv ->
      arbitrary_bool >>= fun no_recv_stp ->
      arbitrary_bool >>= fun no_flood ->
      arbitrary_bool >>= fun no_fwd ->
      arbitrary_bool >>= fun no_packet_in ->
        ret_gen {
          down; no_stp; no_recv; no_recv_stp; no_flood; no_fwd; no_packet_in
        }

    let to_string = PortConfig.to_string

    let marshal = PortConfig.to_int
    let parse = PortConfig.of_int
  end

  module PortState = struct
    open PortDescription

    module StpState = struct
      open PortState

      let arbitrary =
        let open Gen in
        elements StpState.([Listen; Learn; Forward; Block])
    end

    type t = PortState.t
    type s = Int32.t

    let arbitrary =
      let open Gen in
      arbitrary_bool >>= fun down ->
      StpState.arbitrary >>= fun stp_state ->
        let open PortState in
        ret_gen { down; stp_state }

    let to_string = PortState.to_string

    let marshal = PortState.to_int
    let parse = PortState.of_int
  end

  module PortFeatures = struct
    open PortDescription

    type t = PortFeatures.t
    type s = Int32.t

    let arbitrary =
      let open Gen in
      let open PortFeatures in
      arbitrary_bool >>= fun f_10MBHD ->
      arbitrary_bool >>= fun f_10MBFD ->
      arbitrary_bool >>= fun f_100MBHD ->
      arbitrary_bool >>= fun f_100MBFD ->
      arbitrary_bool >>= fun f_1GBHD ->
      arbitrary_bool >>= fun f_1GBFD ->
      arbitrary_bool >>= fun f_10GBFD ->
      arbitrary_bool >>= fun copper ->
      arbitrary_bool >>= fun fiber ->
      arbitrary_bool >>= fun autoneg ->
      arbitrary_bool >>= fun pause ->
      arbitrary_bool >>= fun pause_asym ->
        ret_gen {
          f_10MBHD; f_10MBFD; f_100MBHD; f_100MBFD; f_1GBHD; f_1GBFD; f_10GBFD;
          copper; fiber; autoneg; pause; pause_asym
        }

    let to_string = PortFeatures.to_string

    let marshal = PortFeatures.to_int
    let parse = PortFeatures.of_int
  end

  type t = PortDescription.t
  type s = Cstruct.t

  let arbitrary =
    let open Gen in
    arbitrary_uint16 >>= fun port_no ->
    arbitrary_uint48 >>= fun hw_addr ->
    arbitrary_stringN 16 >>= fun name ->
    PortConfig.arbitrary >>= fun config ->
    PortState.arbitrary >>= fun state ->
    PortFeatures.arbitrary >>= fun curr ->
    PortFeatures.arbitrary >>= fun advertised ->
    PortFeatures.arbitrary >>= fun supported ->
    PortFeatures.arbitrary >>= fun peer ->
      let open PortDescription in
      ret_gen {
        port_no; hw_addr; name; config; state; curr; advertised; supported; peer
      }

  let to_string = PortDescription.to_string

  let marshal = PortDescription.marshal
  let parse = PortDescription.parse

  let size_of = PortDescription.size_of
end

module PortStatus = struct

  module ChangeReason = struct
    open PortStatus

    let arbitrary =
      let open Gen in
      elements ChangeReason.([Add; Delete; Modify])
  end

  type t = PortStatus.t
  type s = Cstruct.t

  let arbitrary =
    let open Gen in
      ChangeReason.arbitrary >>= fun reason ->
      PortDescription.arbitrary >>= fun desc ->
        let open PortStatus in
        ret_gen { reason; desc }

  let to_string = PortStatus.to_string

  let marshal = PortStatus.marshal
  let parse = PortStatus.parse

  let size_of = PortStatus.size_of
end
