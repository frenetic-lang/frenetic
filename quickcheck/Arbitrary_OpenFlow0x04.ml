open OpenFlow0x04
open OpenFlow0x04_Core
open Arbitrary_Base

open QuickCheck
module Gen = QuickCheck_gen


let arbitrary_32mask =
  let open Gen in
  (choose_int (1, 32)) >>= fun a ->
    ret_gen (Int32.of_int a)

let arbitrary_64mask = 
  let open Gen in
  (choose_int (1,64)) >>= fun a ->
    ret_gen (Int64.of_int a)

let arbitrary_48mask =
  let open Gen in
  (choose_int (1,48)) >>= fun a ->
    ret_gen (Int64.of_int a)

let arbitrary_12mask =
  let open Gen in
  (choose_int (1,12)) >>= fun a ->
    ret_gen a

let arbitrary_16mask =
  let open Gen in
  (choose_int (1,16)) >>= fun a ->
    ret_gen a
    
let arbitrary_masked arb arb_mask =
  let open OpenFlow0x04_Core in
  let open Gen in
  frequency [
    (1, arb >>= fun v -> ret_gen {OpenFlow0x04_Core.m_value = v; m_mask = None});
    (3, arb >>= fun v ->
        arb_mask >>= fun m -> ret_gen {OpenFlow0x04_Core.m_value = v; m_mask = Some m}) ]

let arbitrary_oxm =
    let open Gen in
        arbitrary_uint32 >>= fun oxmInPort ->
        arbitrary_uint32 >>= fun oxmInPhyPort ->
        arbitrary_masked arbitrary_uint64 arbitrary_64mask >>= fun oxmMetadata ->
        arbitrary_uint16 >>= fun oxmEthType ->
        arbitrary_masked arbitrary_uint48 arbitrary_48mask >>= fun oxmEthDst ->
        arbitrary_masked arbitrary_uint48 arbitrary_48mask >>= fun oxmEthSrc ->
        arbitrary_masked arbitrary_uint12 arbitrary_12mask >>= fun oxmVlanVId ->
        arbitrary_uint8 >>= fun oxmVlanPcp -> 
        arbitrary_uint8 >>= fun oxmIPProto -> 
        arbitrary_uint8 >>= fun oxmIPDscp -> 
        arbitrary_uint8 >>= fun oxmIPEcn -> 
        arbitrary_masked arbitrary_uint32 arbitrary_32mask >>= fun oxmIP4Src ->
        arbitrary_masked arbitrary_uint32 arbitrary_32mask >>= fun oxmIP4Dst ->
        arbitrary_masked arbitrary_uint16 arbitrary_16mask >>= fun oxmTCPSrc ->
        arbitrary_masked arbitrary_uint16 arbitrary_16mask >>= fun oxmTCPDst ->
        arbitrary_uint16 >>= fun oxmARPOp -> 
        arbitrary_masked arbitrary_uint32 arbitrary_32mask >>= fun oxmARPSpa ->
        arbitrary_masked arbitrary_uint32 arbitrary_32mask >>= fun oxmARPTpa ->
        arbitrary_masked arbitrary_uint48 arbitrary_48mask >>= fun oxmARPSha ->
        arbitrary_masked arbitrary_uint48 arbitrary_48mask >>= fun oxmARPTha ->
        arbitrary_uint8 >>= fun oxmICMPType -> 
        arbitrary_uint8 >>= fun oxmICMPCode -> 
        arbitrary_uint32 >>= fun oxmMPLSLabel -> 
        arbitrary_uint8 >>= fun oxmMPLSTc -> 
        arbitrary_masked arbitrary_uint64 arbitrary_64mask >>= fun oxmTunnelId ->
    oneof [
        ret_gen (OxmInPort oxmInPort);
        ret_gen (OxmInPhyPort oxmInPhyPort);
        ret_gen (OxmMetadata oxmMetadata);
        ret_gen (OxmEthType oxmEthType);
        ret_gen (OxmEthDst oxmEthDst);
        ret_gen (OxmEthSrc oxmEthSrc);
        ret_gen (OxmVlanVId oxmVlanVId);
        ret_gen (OxmVlanPcp oxmVlanPcp);
        ret_gen (OxmIPProto oxmIPProto);
        ret_gen (OxmIPDscp oxmIPDscp);
        ret_gen (OxmIPEcn oxmIPEcn);
        ret_gen (OxmIP4Src oxmIP4Src);
        ret_gen (OxmIP4Dst oxmIP4Dst);
        ret_gen (OxmTCPSrc oxmTCPSrc);
        ret_gen (OxmTCPDst oxmTCPDst);
        ret_gen (OxmARPOp oxmARPOp);
        ret_gen (OxmARPSpa oxmARPSpa);
        ret_gen (OxmARPTpa oxmARPTpa);
        ret_gen (OxmARPSha oxmARPSha);
        ret_gen (OxmARPTha oxmARPTha);
        ret_gen (OxmICMPType oxmICMPType);
        ret_gen (OxmICMPCode oxmICMPCode);
        ret_gen (OxmMPLSLabel oxmMPLSLabel);
        ret_gen (OxmMPLSTc oxmMPLSTc);
        ret_gen (OxmTunnelId oxmTunnelId)
    ]

let arbitrary_oxmMatch = 
    arbitrary_list arbitrary_oxm

module type OpenFlow0x04_Arbitrary = sig

    type t
    type s

    val arbitrary : t arbitrary

    val to_string : t -> string

    val parse : s -> t
    val marshal : t -> s

end

module type OpenFlow0x04_ArbitraryCstruct = sig
  type t

  val arbitrary : t arbitrary

  val to_string : t -> string

  val parse : Cstruct.t -> t
  val marshal : Cstruct.t -> t -> int

  val size_of : t -> int

end

module OpenFlow0x04_Unsize(ArbC : OpenFlow0x04_ArbitraryCstruct) = struct
  type t = ArbC.t
  type s = Cstruct.t

  let arbitrary = ArbC.arbitrary

  let to_string = ArbC.to_string

  let parse = ArbC.parse

  let marshal m =
    let bytes = Cstruct.of_bigarray Bigarray.(Array1.create char c_layout (ArbC.size_of m))
      in ignore (ArbC.marshal bytes m); bytes
end

module PortDesc = struct
  module PortFeatures = struct
    
    type t = OpenFlow0x04_Core.portFeatures
    type s = Int32.t
    
    let arbitrary = 
        let open Gen in
        let open PortFeatures in
        arbitrary_bool >>= fun rate_10mb_hd ->
        arbitrary_bool >>= fun rate_10mb_fd ->
        arbitrary_bool >>= fun rate_100mb_hd ->
        arbitrary_bool >>= fun rate_100mb_fd ->
        arbitrary_bool >>= fun rate_1gb_hd ->
        arbitrary_bool >>= fun rate_1gb_fd ->
        arbitrary_bool >>= fun rate_10gb_fd ->
        arbitrary_bool >>= fun rate_40gb_fd ->
        arbitrary_bool >>= fun rate_100gb_fd ->
        arbitrary_bool >>= fun rate_1tb_fd ->
        arbitrary_bool >>= fun other ->
        arbitrary_bool >>= fun copper ->
        arbitrary_bool >>= fun fiber ->
        arbitrary_bool >>= fun autoneg ->
        arbitrary_bool >>= fun pause ->
        arbitrary_bool >>= fun pause_asym ->
        ret_gen {
            rate_10mb_hd; rate_10mb_fd; 
            rate_100mb_hd; rate_100mb_fd;
            rate_1gb_hd; rate_1gb_fd;
            rate_10gb_fd; rate_40gb_fd;
            rate_100gb_fd; rate_1tb_fd;
            other; copper; fiber;
            autoneg; pause; pause_asym
        }

    let to_string = PortFeatures.to_string
    let marshal = PortFeatures.marshal
    let parse = PortFeatures.parse
  end

  module PortState = struct
    type t = OpenFlow0x04_Core.portState
    type s = Int32.t
    let arbitrary =
        let open Gen in
        let open PortState in
        arbitrary_bool >>= fun link_down ->
        arbitrary_bool >>= fun blocked ->
        arbitrary_bool >>= fun live ->
        ret_gen {
            link_down;
            blocked;
            live
        }
    let to_string = PortState.to_string
    let marshal = PortState.marshal
    let parse = PortState.parse
  end

  module PortConfig = struct
    type t = OpenFlow0x04_Core.portConfig
    type s = Int32.t
    let arbitrary =
        let open Gen in
        let open PortConfig in
        arbitrary_bool >>= fun port_down ->
        arbitrary_bool >>= fun no_recv ->
        arbitrary_bool >>= fun no_fwd ->
        arbitrary_bool >>= fun no_packet_in ->
        ret_gen {
            port_down;
            no_recv;
            no_fwd;
            no_packet_in
        }
    let to_string = PortConfig.to_string
    let marshal = PortConfig.marshal
    let parse = PortConfig.parse
  end
  
  type t = OpenFlow0x04_Core.portDesc
  
  let arbitrary =
    let open Gen in
    let open PortDesc in
    arbitrary_uint32 >>= fun port_no ->
    PortConfig.arbitrary >>= fun config ->
    PortState.arbitrary >>= fun state ->
    PortFeatures.arbitrary >>= fun curr ->
    PortFeatures.arbitrary >>= fun advertised ->
    PortFeatures.arbitrary >>= fun supported ->
    PortFeatures.arbitrary >>= fun peer ->
    ret_gen {
        port_no;
        config;
        state;
        curr;
        advertised;
        supported; 
        peer
    }
  
  let to_string = PortDesc.to_string
  let parse = PortDesc.parse
  let marshal = PortDesc.marshal
  let size_of = PortDesc.sizeof

end

module PortStatus = struct

    open Gen

    type t = OpenFlow0x04_Core.portStatus
    
    let arbitrary_reason =
        oneof [
            ret_gen PortAdd;
            ret_gen PortDelete;
            ret_gen PortModify
        ]


    let arbitrary : t arbitrary =
        let open PortStatus in
        arbitrary_reason >>= fun reason ->
        PortDesc.arbitrary >>= fun desc -> 
            ret_gen {
                reason = reason;
                desc = desc}

    let to_string = PortStatus.to_string
    let parse = PortStatus.parse
    let marshal = PortStatus.marshal
    let size_of = PortStatus.sizeof

end
