open OpenFlow0x04
open OpenFlow0x04_Core
open Arbitrary_Base

open QuickCheck
module Gen = QuickCheck_gen

let sum (lst : int list) = List.fold_left (fun x y -> x + y) 0 lst

let arbitrary_32mask =
  let open Gen in
  (choose_int (1, 32)) >>= fun a ->
    ret_gen (Int32.of_int a)

let arbitrary_128mask =
  let open Gen in
  (choose_int (1,64)) >>= fun a ->
  (choose_int (0,64)) >>= fun b ->
    ret_gen (Int64.of_int b,Int64.of_int a)

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

let arbitrary_timeout =
    let open OpenFlow0x04_Core in
    let open Gen in
    oneof [
        ret_gen Permanent;
        arbitrary_uint16 >>= (fun n -> ret_gen (ExpiresAfter n))
    ]

let fill_with_0 n= 
    String.make n '\000'

let arbitrary_stringl n=
    let open Gen in
    (choose_int (0,n)) >>= fun a ->
    arbitrary_stringN a >>= fun str ->
    ret_gen  (str ^ (fill_with_0 (n-a)))

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
    arbitrary_uint48 >>= fun hw_addr ->
    arbitrary_stringN 16 >>= fun name ->
    PortConfig.arbitrary >>= fun config ->
    PortState.arbitrary >>= fun state ->
    PortFeatures.arbitrary >>= fun curr ->
    PortFeatures.arbitrary >>= fun advertised ->
    PortFeatures.arbitrary >>= fun supported ->
    PortFeatures.arbitrary >>= fun peer ->
    arbitrary_uint32 >>= fun curr_speed ->
    arbitrary_uint32 >>= fun max_speed ->
    ret_gen {
        port_no;
        hw_addr;
        name;
        config;
        state;
        curr;
        advertised;
        supported; 
        peer;
        curr_speed;
        max_speed
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

module PseudoPort = struct
  type s = int * (int option)
  type t = PseudoPort.t

  let arbitrary =
    let open Gen in
    let open OpenFlow0x04_Core in
      oneof [
        arbitrary_uint32 >>= (fun p -> ret_gen (PhysicalPort p));
        ret_gen InPort;
        ret_gen Table;
        ret_gen Normal;
        ret_gen Flood;
        ret_gen AllPorts;
        arbitrary_uint >>= (fun l -> ret_gen (Controller l));
        ret_gen Local;
        ret_gen Any
      ]

  (* Use in cases where a `Controller` port is invalid input *)
  let arbitrary_nc =
    let open Gen in
    let open OpenFlow0x04_Core in
      oneof [
        arbitrary_uint32 >>= (fun p -> ret_gen (PhysicalPort p));
        ret_gen InPort;
        ret_gen Table;
        ret_gen Normal;
        ret_gen Flood;
        ret_gen AllPorts;
        ret_gen Local;
        ret_gen Any
      ]

  let to_string = PseudoPort.to_string

  let parse (p, l) =
    let l' = match l with
             | None   -> 0
             | Some i -> i
      in PseudoPort.make p l'

  let marshal p =
    let open OpenFlow0x04_Core in
    let l = match p with
            | Controller i -> Some i
            | _            -> None
      in (PseudoPort.marshal p, l)
  let size_of = PseudoPort.size_of
end

module OfpMatch = struct
    open Gen
    type t = OpenFlow0x04_Core.oxmMatch

    module Oxm = struct
        type t = Oxm.t
        
        let arbitrary = 
            let open Gen in
            let open Oxm in
            let arbitrary_dscp = 
              (choose_int (0,64)) >>= fun a ->
              ret_gen a in
            let arbitrary_ecn = 
            (choose_int (0,3)) >>= fun a ->
              ret_gen a in
            let arbitrary_24mask =
              let open Gen in
              (choose_int (1,24)) >>= fun a ->
                ret_gen (Int32.of_int a) in
            let arbitrary_uint24 =
              arbitrary_uint16 >>= fun a ->
              arbitrary_uint8 >>= fun b ->
                let open Int32 in
                let hi = shift_left (of_int a) 8 in
                let lo = of_int b in
                ret_gen (logor hi lo) in
            let arbitrary_ipv6hdr =
              arbitrary_bool >>= fun noext ->
              arbitrary_bool >>= fun esp ->
              arbitrary_bool >>= fun auth ->
              arbitrary_bool >>= fun dest ->
              arbitrary_bool >>= fun frac ->
              arbitrary_bool >>= fun router ->
              arbitrary_bool >>= fun hop ->
              arbitrary_bool >>= fun unrep ->
              arbitrary_bool >>= fun unseq ->
              ret_gen {noext; esp; auth; dest; frac; router; hop; unrep; unseq } in
            arbitrary_uint32 >>= fun portId ->
            arbitrary_uint32 >>= fun portPhyId ->
            arbitrary_masked arbitrary_uint64 arbitrary_64mask >>= fun oxmMetadata ->
            arbitrary_uint16 >>= fun oxmEthType ->
            arbitrary_masked arbitrary_uint48 arbitrary_48mask >>= fun oxmEthDst ->
            arbitrary_masked arbitrary_uint48 arbitrary_48mask >>= fun oxmEthSrc ->
            arbitrary_masked arbitrary_uint12 arbitrary_12mask >>= fun oxmVlanVId ->
            arbitrary_uint8 >>= fun oxmVlanPcp ->
            arbitrary_uint8 >>= fun oxmIPProto ->
            arbitrary_dscp >>= fun oxmIPDscp ->
            arbitrary_ecn >>= fun oxmIPEcn ->
            arbitrary_masked arbitrary_uint32 arbitrary_32mask >>= fun oxmIP4Src ->
            arbitrary_masked arbitrary_uint32 arbitrary_32mask >>= fun oxmIP4Dst ->
            arbitrary_uint16 >>= fun oxmTCPSrc ->
            arbitrary_uint16 >>= fun oxmTCPDst ->
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
            arbitrary_masked arbitrary_uint128 arbitrary_128mask >>= fun oxmIPv6Src ->
            arbitrary_masked arbitrary_uint128 arbitrary_128mask >>= fun oxmIPv6Dst ->
            arbitrary_masked arbitrary_uint32 arbitrary_32mask  >>= fun oxmIPv6FLabel ->
            arbitrary_masked arbitrary_uint128 arbitrary_128mask >>= fun oxmIPv6NDTarget ->
            arbitrary_masked arbitrary_uint24 arbitrary_24mask >>= fun oxmPBBIsid ->
            arbitrary_masked arbitrary_ipv6hdr arbitrary_ipv6hdr  >>= fun oxmIPv6ExtHdr ->
            arbitrary_bool >>= fun oxmMPLSBos ->
            arbitrary_uint16 >>= fun oxmUDPSrc ->
            arbitrary_uint16 >>= fun oxmUDPDst ->
            arbitrary_uint16 >>= fun oxmSCTPSrc ->
            arbitrary_uint16 >>= fun oxmSCTPDst ->
            arbitrary_uint8 >>= fun oxmICMPv6Type ->
            arbitrary_uint8 >>= fun oxmICMPv6Code ->
            arbitrary_uint48 >>= fun oxmIPv6NDSll ->
            arbitrary_uint48 >>= fun oxmIPv6NDTll ->
            oneof [
                ret_gen (OxmInPort portId);
                ret_gen (OxmInPhyPort portPhyId);
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
                ret_gen (OxmTunnelId oxmTunnelId);
                ret_gen (OxmUDPSrc oxmUDPSrc);
                ret_gen (OxmUDPDst oxmUDPDst);
                ret_gen (OxmSCTPSrc oxmSCTPSrc);
                ret_gen (OxmSCTPDst oxmSCTPDst);
                ret_gen (OxmIPv6Src oxmIPv6Src);
                ret_gen (OxmIPv6Dst oxmIPv6Dst);
                ret_gen (OxmIPv6FLabel oxmIPv6FLabel);
                ret_gen (OxmICMPv6Type oxmICMPv6Type);
                ret_gen (OxmICMPv6Code oxmICMPv6Code);
                ret_gen (OxmIPv6NDTarget oxmIPv6NDTarget);
                ret_gen (OxmIPv6NDSll oxmIPv6NDSll);
                ret_gen (OxmIPv6NDTll oxmIPv6NDTll);
                ret_gen (OxmMPLSBos oxmMPLSBos);
                ret_gen (OxmPBBIsid oxmPBBIsid);
                ret_gen (OxmIPv6ExtHdr oxmIPv6ExtHdr);
            ]
        let marshal = Oxm.marshal
        let to_string = Oxm.to_string
        let size_of = Oxm.sizeof
        let parse bits = 
            let p,_ = Oxm.parse bits in
            p
    end

    module OxmHeader = struct
        type t = OpenFlow0x04_Core.oxm
        
        module Oxm = OpenFlow0x04.Oxm
        
        let arbitrary = 
            let open Gen in
            let open Oxm in
            let ipv6hdr_nul = {noext = false; esp = false; auth = false; dest = false; frac = false; router = false; hop = false; unrep = false; unseq = false } in
            arbitrary_masked (ret_gen 0L) (ret_gen 0L) >>= fun oxmMetadata ->
            arbitrary_masked (ret_gen 0L) (ret_gen 0L) >>= fun oxmEthDst ->
            arbitrary_masked (ret_gen 0L) (ret_gen 0L) >>= fun oxmEthSrc ->
            arbitrary_masked (ret_gen 0) (ret_gen 0) >>= fun oxmVlanVId ->
            arbitrary_masked (ret_gen 0l) (ret_gen 0l) >>= fun oxmIP4Src ->
            arbitrary_masked (ret_gen 0l) (ret_gen 0l) >>= fun oxmIP4Dst ->
            arbitrary_masked (ret_gen 0l) (ret_gen 0l) >>= fun oxmARPSpa ->
            arbitrary_masked (ret_gen 0l) (ret_gen 0l) >>= fun oxmARPTpa ->
            arbitrary_masked (ret_gen 0L) (ret_gen 0L) >>= fun oxmARPSha ->
            arbitrary_masked (ret_gen 0L) (ret_gen 0L) >>= fun oxmARPTha ->
            arbitrary_masked (ret_gen 0L) (ret_gen 0L) >>= fun oxmTunnelId ->
            arbitrary_masked (ret_gen (0L,0L)) (ret_gen (0L,0L)) >>= fun oxmIPv6Src ->
            arbitrary_masked (ret_gen (0L,0L)) (ret_gen (0L,0L)) >>= fun oxmIPv6Dst ->
            arbitrary_masked (ret_gen 0l) (ret_gen 0l) >>= fun oxmIPv6FLabel ->
            arbitrary_masked (ret_gen (0L,0L)) (ret_gen (0L,0L)) >>= fun oxmIPv6NDTarget ->
            arbitrary_masked (ret_gen 0l) (ret_gen 0l) >>= fun oxmPBBIsid ->
            arbitrary_masked (ret_gen ipv6hdr_nul) (ret_gen ipv6hdr_nul) >>= fun oxmIPv6ExtHdr ->
            
            oneof [
                ret_gen (OxmInPort 0l);
                ret_gen (OxmInPhyPort 0l);
                ret_gen (OxmMetadata oxmMetadata);
                ret_gen (OxmEthType 0);
                ret_gen (OxmEthDst oxmEthDst);
                ret_gen (OxmEthSrc oxmEthSrc);
                ret_gen (OxmVlanVId oxmVlanVId);
                ret_gen (OxmVlanPcp 0);
                ret_gen (OxmIPProto 0);
                ret_gen (OxmIPDscp 0);
                ret_gen (OxmIPEcn 0);
                ret_gen (OxmIP4Src oxmIP4Src);
                ret_gen (OxmIP4Dst oxmIP4Dst);
                ret_gen (OxmTCPSrc 0);
                ret_gen (OxmTCPDst 0);
                ret_gen (OxmARPOp 0);
                ret_gen (OxmARPSpa oxmARPSpa);
                ret_gen (OxmARPTpa oxmARPTpa);
                ret_gen (OxmARPSha oxmARPSha);
                ret_gen (OxmARPTha oxmARPTha);
                ret_gen (OxmICMPType 0);
                ret_gen (OxmICMPCode 0);
                ret_gen (OxmMPLSLabel 0l);
                ret_gen (OxmMPLSTc 0);
                ret_gen (OxmTunnelId oxmTunnelId);
                ret_gen (OxmUDPSrc 0);
                ret_gen (OxmUDPDst 0);
                ret_gen (OxmSCTPSrc 0);
                ret_gen (OxmSCTPDst 0);
                ret_gen (OxmIPv6Src oxmIPv6Src);
                ret_gen (OxmIPv6Dst oxmIPv6Dst);
                ret_gen (OxmIPv6FLabel oxmIPv6FLabel);
                ret_gen (OxmICMPv6Type 0);
                ret_gen (OxmICMPv6Code 0);
                ret_gen (OxmIPv6NDTarget oxmIPv6NDTarget);
                ret_gen (OxmIPv6NDSll 0L);
                ret_gen (OxmIPv6NDTll 0L);
                ret_gen (OxmMPLSBos false);
                ret_gen (OxmPBBIsid oxmPBBIsid);
                ret_gen (OxmIPv6ExtHdr oxmIPv6ExtHdr);
            ]

        let marshal = Oxm.marshal_header

        let to_string = Oxm.field_name
        let size_of = Oxm.sizeof
        let parse bits = 
            let p,_ = Oxm.parse_header bits in
            p
    end

    let arbitrary =
        let open Gen in
        let open OfpMatch in
        arbitrary_list Oxm.arbitrary >>= fun ofpMatch ->
        ret_gen ofpMatch
    
    let marshal = OfpMatch.marshal
    let parse bits= 
        let ofpMatch,_ = OfpMatch.parse bits in
        ofpMatch
    let to_string = OfpMatch.to_string
    let size_of = OfpMatch.sizeof
end

module MeterBand = struct

  type t = OpenFlow0x04_Core.meterBand

  let arbitrary =
    let open Gen in
    let open OpenFlow0x04_Core in
    arbitrary_uint32 >>= fun rate ->
    arbitrary_uint32 >>= fun burst ->
    oneof [
      ret_gen (Drop (rate, burst));
      arbitrary_uint8 >>= (fun p -> ret_gen (DscpRemark (rate,burst,p)));
      arbitrary_uint32 >>= (fun p -> ret_gen (ExpMeter (rate,burst,p)))
    ]

  let to_string = MeterBand.to_string

  let marshal = MeterBand.marshal
  let parse = MeterBand.parse

  let size_of = MeterBand.sizeof

end

module Action = struct
  type t = OpenFlow0x04_Core.action

  let arbitrary =
    let open Gen in
    let open OpenFlow0x04_Core in
    oneof [
      PseudoPort.arbitrary >>= (fun p -> ret_gen (Output p));
      arbitrary_uint32 >>= (fun p -> ret_gen (Group p));
      ret_gen PopVlan;
      ret_gen PushVlan;
      ret_gen PopMpls;
      ret_gen PushMpls;
      ret_gen CopyTtlOut;
      ret_gen CopyTtlIn;
      ret_gen DecNwTtl;
      ret_gen PushPbb;
      ret_gen PopPbb;
      ret_gen DecMplsTtl;
      arbitrary_uint8 >>= (fun p -> ret_gen (SetNwTtl p));
      arbitrary_uint8 >>= (fun p -> ret_gen (SetMplsTtl p));
      arbitrary_uint32 >>= (fun p -> ret_gen (SetQueue p));
      OfpMatch.Oxm.arbitrary >>= (fun p -> ret_gen (SetField p))
    ]

  let to_string = Action.to_string

  let marshal = Action.marshal
  let parse = Action.parse

  let size_of = Action.sizeof

end

module Instructions = struct
    open Gen
    type t = OpenFlow0x04_Core.instruction list
    
    module Instruction = struct
        type t = OpenFlow0x04_Core.instruction
        
        let arbitrary = 
            let open Gen in
            let open Instruction in
            arbitrary_uint8 >>= fun tableid ->
            arbitrary_uint32 >>= fun meter ->
            arbitrary_uint32 >>= fun exp ->
            arbitrary_masked arbitrary_uint64 arbitrary_64mask >>= fun wrMeta ->
            arbitrary_list Action.arbitrary >>= fun wrAction ->
            arbitrary_list Action.arbitrary >>= fun appAction ->
            oneof [
            ret_gen (GotoTable tableid);
            ret_gen (WriteMetadata wrMeta);
            ret_gen (WriteActions wrAction);
            ret_gen (ApplyActions appAction);
            ret_gen Clear;
            ret_gen (Meter meter);
            ret_gen (Experimenter exp);
            ]

        let marshal = Instruction.marshal
        let parse = Instruction.parse
        let to_string = Instruction.to_string
        let size_of = Instruction.sizeof
    end
    
    let arbitrary =
        let open Gen in
        let open Instructions in
        arbitrary_list Instruction.arbitrary >>= fun ins ->
        ret_gen ins
    
    let marshal = Instructions.marshal
    let parse = Instructions.parse
    let to_string = Instructions.to_string
    let size_of = Instructions.sizeof    
end

module FlowMod = struct
    open Gen
    module FlowModCommand = struct
        type t = OpenFlow0x04_Core.flowModCommand

        let arbitrary =
            let open Gen in
            let open FlowModCommand in
            oneof [
                        ret_gen AddFlow;
                        ret_gen ModFlow;
                        ret_gen ModStrictFlow;
                        ret_gen DeleteFlow;
                        ret_gen DeleteStrictFlow;
                    ]
        let to_string = FlowModCommand.to_string
        let marshal = FlowModCommand.marshal
        let parse = FlowModCommand.parse
    end
    type t = OpenFlow0x04_Core.flowMod

    let arbitrary_flags =
        arbitrary_bool >>= fun fmf_send_flow_rem ->
        arbitrary_bool >>= fun fmf_check_overlap ->
        arbitrary_bool >>= fun fmf_reset_counts ->
        arbitrary_bool >>= fun fmf_no_pkt_counts ->
        arbitrary_bool >>= fun fmf_no_byt_counts ->
        ret_gen {
            fmf_send_flow_rem;
            fmf_check_overlap;
            fmf_reset_counts;
            fmf_no_pkt_counts;
            fmf_no_byt_counts
        }

    let arbitrary_buffer_id = 
        arbitrary_uint32 >>= fun bid ->
        oneof [
            ret_gen None;
            ret_gen (Some bid)
        ]

    let arbitrary = 
        arbitrary_masked arbitrary_uint64 arbitrary_64mask >>= fun mfCookie ->
        arbitrary_uint8 >>= fun mfTable_id ->
        arbitrary_timeout >>= fun mfIdle_timeout ->
        arbitrary_timeout >>= fun mfHard_timeout ->
        arbitrary_uint16 >>= fun mfPriority ->
        arbitrary_flags >>= fun mfFlags ->
        arbitrary_buffer_id >>= fun mfBuffer_id ->
        FlowModCommand.arbitrary >>= fun mfCommand ->
        PseudoPort.arbitrary_nc >>= fun mfPort ->
        oneof [ ret_gen None; ret_gen (Some mfPort)] >>= fun mfOut_port ->
        arbitrary_uint32 >>= fun mfGroup ->
        oneof [ ret_gen None; ret_gen (Some mfGroup)] >>= fun mfOut_group ->
        OfpMatch.arbitrary >>= fun mfOfp_match ->
        Instructions.arbitrary >>= fun mfInstructions ->
        ret_gen {
            mfCookie; mfTable_id;
            mfCommand; mfIdle_timeout;
            mfHard_timeout; mfPriority;
            mfBuffer_id;
            mfOut_port;
            mfOut_group; mfFlags;
            mfOfp_match; mfInstructions}
        
    let marshal = FlowMod.marshal
    let parse = FlowMod.parse
    let to_string = FlowMod.to_string
    let size_of = FlowMod.sizeof
end    

module Bucket = struct
  open Gen
  open OpenFlow0x04_Core

  type t = OpenFlow0x04_Core.bucket

  let arbitrary_option =
     frequency [
    (1, ret_gen None);
    (5, arbitrary_uint32 >>= (fun v -> ret_gen (Some v)))
    ]

  let no_output act = 
    match act with
      | Output _ -> false
      | _ -> true

  let arbitrary =
    arbitrary_uint16 >>= fun bu_weight ->
    arbitrary_option >>= fun bu_watch_port ->
    arbitrary_option >>= fun bu_watch_group ->
    list1 (such_that no_output Action.arbitrary) >>= fun bu_actions ->
    ret_gen {
       bu_weight; 
       bu_watch_port; 
       bu_watch_group; 
       bu_actions
    }
    
  let marshal = Bucket.marshal
  let parse = Bucket.parse
  let to_string = Bucket.to_string
  let size_of = Bucket.sizeof
end

module MultipartReq = struct
  open Gen
  open OpenFlow0x04_Core
  module TableFeatures = struct
    module TableFeatureProp = struct
      
      type t = TableFeatureProp.t
      
      let arbitrary = 
        oneof [
          Instructions.arbitrary >>= (fun n -> ret_gen (TfpInstruction n));
          Instructions.arbitrary >>= (fun n -> ret_gen (TfpInstructionMiss n));
          arbitrary_list Action.arbitrary >>= (fun n -> ret_gen (TfpWriteAction n));
          arbitrary_list Action.arbitrary >>= (fun n -> ret_gen (TfpWriteActionMiss n));
          arbitrary_list Action.arbitrary >>= (fun n -> ret_gen (TfpApplyAction n));
          arbitrary_list Action.arbitrary >>= (fun n -> ret_gen (TfpApplyActionMiss n));
          arbitrary_list OfpMatch.OxmHeader.arbitrary >>= (fun n -> ret_gen (TfpMatch n));
          arbitrary_list OfpMatch.OxmHeader.arbitrary >>= (fun n -> ret_gen (TfpWildcard n));
          arbitrary_list OfpMatch.OxmHeader.arbitrary >>= (fun n -> ret_gen (TfpWriteSetField n));
          arbitrary_list OfpMatch.OxmHeader.arbitrary >>= (fun n -> ret_gen (TfpWriteSetFieldMiss n));
          arbitrary_list OfpMatch.OxmHeader.arbitrary >>= (fun n -> ret_gen (TfpApplySetField n));
          arbitrary_list OfpMatch.OxmHeader.arbitrary >>= (fun n -> ret_gen (TfpApplySetFieldMiss n))
          ]

      let marshal = TableFeatureProp.marshal
      let parse = TableFeatureProp.parse
      let to_string = TableFeatureProp.to_string
      let size_of = TableFeatureProp.sizeof
    end
    
    module TableFeature = struct
      type t = TableFeature.t

      let arbitrary_config =
        ret_gen Deprecated

      let calc_length tfp =
        (* sizeof_ofp_table_feature = 64*)
        ret_gen (64+(TableFeatureProp.size_of tfp))

      let arbitrary = 
        arbitrary_uint8 >>= fun table_id ->
        arbitrary_stringN 32 >>= fun name ->
        arbitrary_uint64 >>= fun metadata_match ->
        arbitrary_uint64 >>= fun metadata_write ->
        arbitrary_config >>= fun config ->
        arbitrary_uint32 >>= fun max_entries ->
        TableFeatureProp.arbitrary >>= fun feature_prop ->
        calc_length feature_prop>>= fun length ->
        ret_gen {
          length;
          table_id;
          name;
          metadata_match;
          metadata_write;
          config;
          max_entries;
          feature_prop
        }
      
      let marshal = TableFeature.marshal
      let parse bits= 
            let p,_ = TableFeature.parse bits in
            p
      let to_string = TableFeature.to_string
      let size_of = TableFeature.sizeof
    end

    type t = TableFeatures.t

    let arbitrary =
        list1 TableFeature.arbitrary >>= fun v ->
        ret_gen v
    let marshal = TableFeatures.marshal
    let parse = TableFeatures.parse
    let to_string = TableFeatures.to_string
    let size_of = TableFeatures.sizeof
  end

  module FlowRequest = struct
    type t = FlowRequest.t
    
    let arbitrary =
        arbitrary_uint8 >>= fun fr_table_id ->
        arbitrary_uint32 >>= fun fr_out_port ->
        arbitrary_uint32 >>= fun fr_out_group ->
        arbitrary_masked arbitrary_uint64 arbitrary_64mask >>= fun fr_cookie ->
        OfpMatch.arbitrary >>= fun fr_match ->
        ret_gen {
        fr_table_id;
        fr_out_port;
        fr_out_group;
        fr_cookie;
        fr_match
        }
    let marshal = FlowRequest.marshal
    let parse = FlowRequest.parse
    let to_string = FlowRequest.to_string
    let size_of = FlowRequest.sizeof
  end

  module QueueRequest = struct
    type t = QueueRequest.t

    let arbitrary = 
        arbitrary_uint32 >>= fun port_number ->
        arbitrary_uint32 >>= fun queue_id ->
        ret_gen {
            port_number;
            queue_id
        }

    let marshal = QueueRequest.marshal
    let parse = QueueRequest.parse
    let to_string = QueueRequest.to_string
    let size_of = QueueRequest.sizeof
  end
  
  type t = MultipartReq.t
  
  let arbitrary_option =
     frequency [
    (1, ret_gen None);
    (3, TableFeatures.arbitrary >>= (fun v -> ret_gen (Some v)))
    ]
  
  let arbitrary_type = 
    oneof [
        ret_gen SwitchDescReq;
        ret_gen PortsDescReq;
        FlowRequest.arbitrary >>= (fun n -> ret_gen (FlowStatsReq n));
        FlowRequest.arbitrary >>= (fun n -> ret_gen (AggregFlowStatsReq n));
        ret_gen TableStatsReq;
        arbitrary_uint32 >>= (fun n -> ret_gen (PortStatsReq n));
        QueueRequest.arbitrary >>= (fun n -> ret_gen (QueueStatsReq n));
        arbitrary_uint32 >>= (fun n -> ret_gen (GroupStatsReq n));
        ret_gen GroupDescReq;
        ret_gen GroupFeatReq;
        arbitrary_uint32 >>= (fun n -> ret_gen (MeterStatsReq n));
        arbitrary_uint32 >>= (fun n -> ret_gen (MeterConfReq n));
        ret_gen MeterFeatReq;
        arbitrary_option >>= (fun n -> ret_gen (TableFeatReq n));
    ]
  let arbitrary =
    arbitrary_bool >>= fun mpr_flags ->
    arbitrary_type >>= fun mpr_type ->
    ret_gen {
        mpr_type;
        mpr_flags
    }
  
  let marshal = MultipartReq.marshal
  let parse = MultipartReq.parse
  let to_string = MultipartReq.to_string
  let size_of = MultipartReq.sizeof
end

module MultipartReply = struct
  open Gen
  open OpenFlow0x04_Core
    
  module FlowStats = struct
    type t = FlowStats.t

    let arbitrary_flags =
        arbitrary_bool >>= fun fmf_send_flow_rem ->
        arbitrary_bool >>= fun fmf_check_overlap ->
        arbitrary_bool >>= fun fmf_reset_counts ->
        arbitrary_bool >>= fun fmf_no_pkt_counts ->
        arbitrary_bool >>= fun fmf_no_byt_counts ->
        ret_gen {
        fmf_send_flow_rem;
        fmf_check_overlap;
        fmf_reset_counts;
        fmf_no_pkt_counts;
        fmf_no_byt_counts
        }

    let arbitrary =
        Instructions.arbitrary >>= fun instructions ->
        arbitrary_uint8 >>= fun table_id ->
        list1 OfpMatch.Oxm.arbitrary >>= fun ofp_match ->
        arbitrary_uint64 >>= fun byte_count ->
        arbitrary_uint64 >>= fun packet_count ->
        arbitrary_uint64 >>= fun cookie ->
        arbitrary_uint32 >>= fun duration_sec ->
        arbitrary_uint32 >>= fun duration_nsec ->
        arbitrary_uint16 >>= fun priority ->
        arbitrary_timeout >>= fun idle_timeout ->
        arbitrary_timeout >>= fun hard_timeout ->
        arbitrary_flags >>= fun flags ->
        ret_gen { table_id
                ; duration_sec
                ; duration_nsec
                ; priority
                ; idle_timeout
                ; hard_timeout
                ; flags
                ; cookie
                ; packet_count
                ; byte_count
                ; ofp_match
                ; instructions}

    let marshal = FlowStats.marshal
    let parse = FlowStats.parse
    let to_string = FlowStats.to_string
    let size_of = FlowStats.sizeof
  end
  
  module AggregateStats = struct
    type t = AggregateStats.t
    
    let arbitrary =
        arbitrary_uint64 >>= fun packet_count ->
        arbitrary_uint64 >>= fun byte_count ->
        arbitrary_uint32 >>= fun flow_count ->
        ret_gen {
            packet_count;
            byte_count;
            flow_count
        }
    
    let marshal = AggregateStats.marshal
    let parse = AggregateStats.parse
    let to_string = AggregateStats.to_string
    let size_of = AggregateStats.sizeof
  end
  
  module TableStats = struct
    type t = TableStats.t
    
    let arbitrary =
        arbitrary_uint8 >>= fun table_id ->
        arbitrary_uint32 >>= fun active_count ->
        arbitrary_uint64 >>= fun lookup_count ->
        arbitrary_uint64 >>= fun matched_count ->
        ret_gen {
            table_id;
            active_count;
            lookup_count;
            matched_count
        }

    let marshal = TableStats.marshal
    let parse = TableStats.parse
    let to_string = TableStats.to_string
    let size_of = TableStats.sizeof
  end

  module PortStats = struct

    type t = PortStats.t
    
    let arbitrary =
        arbitrary_uint32 >>= fun psPort_no ->
        arbitrary_uint64 >>= fun rx_packets ->
        arbitrary_uint64 >>= fun tx_packets ->
        arbitrary_uint64 >>= fun rx_bytes ->
        arbitrary_uint64 >>= fun tx_bytes ->
        arbitrary_uint64 >>= fun rx_dropped ->
        arbitrary_uint64 >>= fun tx_dropped ->
        arbitrary_uint64 >>= fun rx_errors ->
        arbitrary_uint64 >>= fun tx_errors ->
        arbitrary_uint64 >>= fun rx_frame_err ->
        arbitrary_uint64 >>= fun rx_over_err ->
        arbitrary_uint64 >>= fun rx_crc_err ->
        arbitrary_uint64 >>= fun collisions ->
        arbitrary_uint32 >>= fun duration_sec ->
        arbitrary_uint32 >>= fun duration_nsec ->
        ret_gen {
            psPort_no;
            rx_packets;
            tx_packets;
            rx_bytes;
            tx_bytes;
            rx_dropped;
            tx_dropped;
            rx_errors;
            tx_errors;
            rx_frame_err;
            rx_over_err;
            rx_crc_err;
            collisions;
            duration_sec;
            duration_nsec
        }

    let marshal = PortStats.marshal
    let parse = PortStats.parse
    let to_string = PortStats.to_string
    let size_of = PortStats.sizeof
  end

  module SwitchDescriptionReply = struct
    type t = SwitchDescriptionReply.t
    
    let arbitrary = 
        arbitrary_stringl 256 >>= fun mfr_desc ->
        arbitrary_stringl 256 >>= fun hw_desc ->
        arbitrary_stringl 256 >>= fun sw_desc ->
        arbitrary_stringl 32 >>= fun serial_num ->
        ret_gen {
            mfr_desc;
            hw_desc;
            sw_desc;
            serial_num
        }
    
    let marshal = SwitchDescriptionReply.marshal
    let parse = SwitchDescriptionReply.parse
    let to_string = SwitchDescriptionReply.to_string
    let size_of = SwitchDescriptionReply.sizeof
  end

  module QueueStats = struct

    type t = QueueStats.t

    let arbitrary =
        arbitrary_uint32 >>= fun qsPort_no ->
        arbitrary_uint32 >>= fun queue_id ->
        arbitrary_uint64 >>= fun tx_bytes ->
        arbitrary_uint64 >>= fun tx_packets ->
        arbitrary_uint64 >>= fun tx_errors ->
        arbitrary_uint32 >>= fun duration_sec ->
        arbitrary_uint32 >>= fun duration_nsec ->
        ret_gen { 
            qsPort_no;
            queue_id;
            tx_bytes;
            tx_packets;
            tx_errors;
            duration_sec;
            duration_nsec
        }

    let marshal = QueueStats.marshal
    let parse = QueueStats.parse
    let to_string = QueueStats.to_string
    let size_of = QueueStats.sizeof


  end

  module GroupStats = struct

    module BucketStats = struct

        type t = GroupStats.BucketStats.t

        let arbitrary =
            arbitrary_uint64 >>= fun packet_count ->
            arbitrary_uint64 >>= fun byte_count ->
            ret_gen {packet_count; byte_count}

        let marshal = GroupStats.BucketStats.marshal
        let parse = GroupStats.BucketStats.parse
        let to_string = GroupStats.BucketStats.to_string
        let size_of = GroupStats.BucketStats.sizeof
    end
  
    type t = GroupStats.t

    let calc_length bs =
        (* sizeof_ofp_group_stats = 40*)
        ret_gen (40+(sum (List.map BucketStats.size_of bs)))

    let arbitrary =
        arbitrary_uint32 >>= fun group_id ->
        arbitrary_uint32 >>= fun ref_count ->
        arbitrary_uint64 >>= fun packet_count ->
        arbitrary_uint64 >>= fun byte_count ->
        arbitrary_uint32 >>= fun duration_sec ->
        arbitrary_uint32 >>= fun duration_nsec ->
        list1 BucketStats.arbitrary >>= fun bucket_stats ->
        calc_length bucket_stats >>= fun length ->
        ret_gen {
            length;
            group_id;
            ref_count;
            packet_count;
            byte_count;
            duration_sec;
            duration_nsec;
            bucket_stats}

    let marshal = GroupStats.marshal
    let parse = GroupStats.parse
    let to_string = GroupStats.to_string
    let size_of = GroupStats.sizeof
  end

  module GroupDesc = struct
  
    type t = GroupDesc.t

    let arbitrary_groupTyp =
      oneof [
        ret_gen All;
        ret_gen Select;
        ret_gen Indirect;
        ret_gen FF]

    let calc_length bucket =
      (* ofp_group_desc = 8*)
      ret_gen (8+ sum (List.map Bucket.size_of bucket))
      
    let arbitrary =
      arbitrary_uint32 >>= fun group_id ->
      arbitrary_groupTyp >>= fun typ ->
      list1 Bucket.arbitrary >>= fun bucket ->
      calc_length bucket>>= fun length ->
      ret_gen {
        length;
        typ;
        group_id;
        bucket
      }

    let marshal = GroupDesc.marshal
    let parse = GroupDesc.parse
    let to_string = GroupDesc.to_string
    let size_of = GroupDesc.sizeof
  end
  module GroupFeatures = struct
  
    type t = GroupFeatures.t
    
    let arbitrary_groupTypeMap =
      arbitrary_bool >>= fun all ->
      arbitrary_bool >>= fun select ->
      arbitrary_bool >>= fun indirect ->
      arbitrary_bool >>= fun ff ->
      ret_gen {
        all;
        select;
        indirect;
        ff
    }

    let arbitrary_groupCapabilities =
      arbitrary_bool >>= fun select_weight ->
      arbitrary_bool >>= fun select_liveness ->
      arbitrary_bool >>= fun chaining ->
      arbitrary_bool >>= fun chaining_checks ->
      ret_gen {
        select_weight;
        select_liveness;
        chaining;
        chaining_checks
      }

    let arbitrary_actionTypeMap =
      arbitrary_bool >>= fun output ->
      arbitrary_bool >>= fun copy_ttl_out ->
      arbitrary_bool >>= fun copy_ttl_in ->
      arbitrary_bool >>= fun set_mpls_ttl ->
      arbitrary_bool >>= fun dec_mpls_ttl ->
      arbitrary_bool >>= fun push_vlan ->
      arbitrary_bool >>= fun pop_vlan ->
      arbitrary_bool >>= fun push_mpls ->
      arbitrary_bool >>= fun pop_mpls ->
      arbitrary_bool >>= fun set_queue ->
      arbitrary_bool >>= fun group ->
      arbitrary_bool >>= fun set_nw_ttl ->
      arbitrary_bool >>= fun dec_nw_ttl ->
      arbitrary_bool >>= fun set_field ->
      arbitrary_bool >>= fun push_pbb ->
      arbitrary_bool >>= fun pop_pbb ->
      ret_gen {
        output;
        copy_ttl_out;
        copy_ttl_in;
        set_mpls_ttl;
        dec_mpls_ttl;
        push_vlan;
        pop_vlan;
        push_mpls;
        pop_mpls;
        set_queue;
        group;
        set_nw_ttl;
        dec_nw_ttl;
        set_field;
        push_pbb;
        pop_pbb
      }
      

    let arbitrary = 
      arbitrary_groupTypeMap >>= fun typ ->
      arbitrary_groupCapabilities >>= fun capabilities ->
      arbitrary_uint32 >>= fun max_groups_all ->
      arbitrary_uint32 >>= fun max_groups_select ->
      arbitrary_uint32 >>= fun max_groups_indirect ->
      arbitrary_uint32 >>= fun max_groups_ff ->
      arbitrary_actionTypeMap >>= fun actions_all ->
      arbitrary_actionTypeMap >>= fun actions_select ->
      arbitrary_actionTypeMap >>= fun actions_indirect ->
      arbitrary_actionTypeMap >>= fun actions_ff ->
      ret_gen {
        typ;
        capabilities;
        max_groups_all;
        max_groups_select;
        max_groups_indirect;
        max_groups_ff;
        actions_all;
        actions_select;
        actions_indirect;
        actions_ff
      }

    let marshal = GroupFeatures.marshal
    let parse = GroupFeatures.parse
    let to_string = GroupFeatures.to_string
    let size_of = GroupFeatures.sizeof
  end
  
  module MeterStats = struct
  
    type t = MeterStats.t

    let calc_length band =
      (* sizeof_ofp_meter_stats = 40*)
      ret_gen (40+(List.length band)*16)

    let arbitrary_meterBandStats =
      arbitrary_uint64 >>= fun packet_band_count ->
      arbitrary_uint64 >>= fun byte_band_count ->
      ret_gen { packet_band_count; byte_band_count }

    let arbitrary =
      arbitrary_uint32 >>= fun meter_id ->
      arbitrary_uint32 >>= fun flow_count ->
      arbitrary_uint64 >>= fun packet_in_count ->
      arbitrary_uint64 >>= fun byte_in_count ->
      arbitrary_uint32 >>= fun duration_sec ->
      arbitrary_uint32 >>= fun duration_nsec ->
      list1 arbitrary_meterBandStats >>= fun band ->
      calc_length band >>= fun len ->
      ret_gen {
        meter_id;
        len;
        flow_count;
        packet_in_count;
        byte_in_count;
        duration_sec;
        duration_nsec;
        band
      }

    let marshal = MeterStats.marshal
    let parse = MeterStats.parse
    let to_string = MeterStats.to_string
    let size_of = MeterStats.sizeof
  end

  module MeterConfig = struct
    type t = MeterConfig.t

    let arbitrary_meterFlagsMap =
      arbitrary_bool >>= fun kbps ->
      arbitrary_bool >>= fun pktps ->
      arbitrary_bool >>= fun burst ->
      arbitrary_bool >>= fun stats ->
      ret_gen {
        kbps;
        pktps;
        burst;
        stats
      }

    let calc_length bands =
      (* sizeof_ofp_meter_config = 8*)
      ret_gen (8 + sum (List.map MeterBand.size_of bands))

    let arbitrary = 
      arbitrary_meterFlagsMap >>= fun flags ->
      arbitrary_uint32 >>= fun meter_id ->
      list1 MeterBand.arbitrary >>= fun bands ->
      calc_length bands >>= fun length ->
      ret_gen {
        length;
        flags;
        meter_id;
        bands
      }

    let marshal = MeterConfig.marshal
    let parse = MeterConfig.parse
    let to_string = MeterConfig.to_string
    let size_of = MeterConfig.sizeof
  end

  module MeterFeaturesStats = struct
    type t = MeterFeaturesStats.t
    
    let arbitrary_meterBandMaps =
      arbitrary_bool >>= fun drop ->
      arbitrary_bool >>= fun dscpRemark ->
      ret_gen {
        drop;
        dscpRemark }

    let arbitrary_meterFlagsMap =
      arbitrary_bool >>= fun kbps ->
      arbitrary_bool >>= fun pktps ->
      arbitrary_bool >>= fun burst ->
      arbitrary_bool >>= fun stats ->
      ret_gen {
        kbps;
        pktps;
        burst;
        stats
      }

    let arbitrary = 
      arbitrary_uint32 >>= fun max_meter ->
      arbitrary_meterBandMaps >>= fun band_typ ->
      arbitrary_meterFlagsMap >>= fun capabilities ->
      arbitrary_uint8 >>= fun max_band ->
      arbitrary_uint8 >>= fun max_color ->
      ret_gen {
        max_meter;
        band_typ;
        capabilities;
        max_band;
        max_color
      }

    let marshal = MeterFeaturesStats.marshal
    let parse = MeterFeaturesStats.parse
    let to_string = MeterFeaturesStats.to_string
    let size_of = MeterFeaturesStats.sizeof
  end

  type t = MultipartReply.t

  let arbitrary =
      arbitrary_bool >>= fun flags ->
      oneof [
          list1 PortDesc.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (PortsDescReply n); mpreply_flags = flags});
          SwitchDescriptionReply.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (SwitchDescReply n); mpreply_flags = flags});
          list1 FlowStats.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (FlowStatsReply n); mpreply_flags = flags});
          AggregateStats.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (AggregateReply n); mpreply_flags = flags});
          list1 TableStats.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (TableReply n); mpreply_flags = flags});
          list1 PortStats.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (PortStatsReply n); mpreply_flags = flags});
          list1 QueueStats.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (QueueStatsReply n);  mpreply_flags = flags});
          list1 GroupStats.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (GroupStatsReply n);  mpreply_flags = flags});
          GroupFeatures.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (GroupFeaturesReply n);  mpreply_flags = flags});
          list1 GroupDesc.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (GroupDescReply n);  mpreply_flags = flags});
          list1 MeterStats.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (MeterReply n);  mpreply_flags = flags});
          list1 MeterConfig.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (MeterConfig n);  mpreply_flags = flags});
          MeterFeaturesStats.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (MeterFeaturesReply n);  mpreply_flags = flags});
          ]

  let marshal = MultipartReply.marshal
  let parse = MultipartReply.parse
  let to_string = MultipartReply.to_string
  let size_of = MultipartReply.sizeof
end

module PacketOut = struct
  open Gen
  open OpenFlow0x04_Core

  type t = PacketOut.t

  let arbitrary_len =
    (choose_int (24, 1500)) >>= fun a ->
    ret_gen a

  let arbitrary_byte n =
  (* construct an arbitrary byte of length n*)
    arbitrary_stringN n  >>= fun a ->
    let byte = Cstruct.create n in
    Cstruct.blit_from_string a 0 byte 0 n;
    ret_gen (byte)

  let arbitrary_pay byte = 
    frequency [
      (1, ret_gen (NotBuffered byte));
      (3, arbitrary_uint32 >>= fun bid ->
          arbitrary_byte 0 >>= fun byte -> (* buffered packet out don't have payload *)
          ret_gen (Buffered (bid,byte)))
    ]

  let arbitrary_port_id = 
    frequency [
      (1, ret_gen None);
      (9, arbitrary_uint32 >>= fun port_id ->
          ret_gen (Some port_id))
    ]

  let arbitrary = 
    arbitrary_list Action.arbitrary >>= fun po_actions ->
    arbitrary_len >>= fun len ->
    arbitrary_byte len >>= fun byte ->
    arbitrary_pay byte >>= fun po_payload ->
    arbitrary_port_id >>= fun po_port_id ->
    ret_gen {
      po_payload;
      po_port_id;
      po_actions
    }

  let parse = PacketOut.parse
  let marshal = PacketOut.marshal
  let to_string = PacketOut.to_string
  let size_of = PacketOut.sizeof
end

module PacketIn = struct
  open Gen
  open OpenFlow0x04_Core

  type t = OpenFlow0x04_Core.packetIn

  let arbitrary_len =
      (choose_int (24, 1500)) >>= fun a ->
       ret_gen a

  let arbitrary_byte n =
  (* construct an arbitrary byte of length n*)
      arbitrary_stringN n  >>= fun a ->
      let byte = Cstruct.create n in
      Cstruct.blit_from_string a 0 byte 0 n;
      ret_gen (byte)

  let arbitrary_reason =
      oneof [
          ret_gen (OpenFlow0x04_Core.NoMatch);
          ret_gen (OpenFlow0x04_Core.ExplicitSend);
          ret_gen (OpenFlow0x04_Core.InvalidTTL)
      ]

  let arbitrary_pay byte = 
      frequency [
          (1, ret_gen (NotBuffered byte));
          (3, arbitrary_uint32 >>= fun bid ->
              ret_gen (Buffered (bid,byte)))
      ]

  let arbitrary =
      arbitrary_len  >>= fun pi_total_len ->
      arbitrary_reason >>= fun pi_reason ->
      arbitrary_uint8 >>= fun pi_table_id ->
      arbitrary_uint64 >>= fun pi_cookie ->
      OfpMatch.arbitrary >>= fun pi_ofp_match ->
      arbitrary_byte pi_total_len >>= fun byte ->
      arbitrary_pay byte >>= fun pi_payload ->
      ret_gen {
          pi_total_len;
          pi_reason;
          pi_table_id;
          pi_cookie;
          pi_ofp_match;
          pi_payload
      }
  

  let marshal = PacketIn.marshal
  let parse = PacketIn.parse
  let to_string = PacketIn.to_string
  let size_of = PacketIn.sizeof

end

module PortMod = struct
  open Gen
  open OpenFlow0x04_Core

  type t = PortMod.t

  let arbitrary = 
    arbitrary_uint32 >>= fun mpPortNo ->
    arbitrary_uint48 >>= fun mpHw_addr ->
    PortDesc.PortConfig.arbitrary >>= fun mpConfig ->
    PortDesc.PortConfig.arbitrary >>= fun mpMask ->
    PortDesc.PortState.arbitrary >>= fun mpAdvertise ->
    ret_gen { mpPortNo; mpHw_addr; mpConfig; mpMask; mpAdvertise}
    

  let marshal = PortMod.marshal
  let parse = PortMod.parse
  let to_string = PortMod.to_string
  let size_of = PortMod.sizeof

end

module MeterMod = struct
  open Gen
  open OpenFlow0x04_Core

  type t = MeterMod.t

  let arbitrary_command = 
    oneof [
      ret_gen AddMeter;
      ret_gen ModifyMeter;
      ret_gen DeleteMeter
    ]

      let arbitrary_meterFlagsMap =
      arbitrary_bool >>= fun kbps ->
      arbitrary_bool >>= fun pktps ->
      arbitrary_bool >>= fun burst ->
      arbitrary_bool >>= fun stats ->
      ret_gen {
        kbps;
        pktps;
        burst;
        stats
      }

  let arbitrary = 
    arbitrary_command >>= fun command ->
    arbitrary_meterFlagsMap >>= fun flags ->
    arbitrary_uint32 >>= fun meter_id ->
    list1 MeterBand.arbitrary >>= fun bands ->
    ret_gen {
      command;
      flags;
      meter_id;
      bands
    }

  
  let marshal = MeterMod.marshal
  let parse = MeterMod.parse
  let to_string = MeterMod.to_string
  let size_of = MeterMod.sizeof

end
  
module Hello = struct
  open Gen
  open OpenFlow0x04_Core

  module Element = struct
    open Gen
    open OpenFlow0x04_Core

    module VersionBitMap = struct
      open Gen
      open OpenFlow0x04_Core
      
      type t = Hello.Element.VersionBitMap.t

      let maxi = 300

      let choose_int2 b =
        choose_int (b,maxi)

      let rec arbitrary_sorted n l acc=
        match n with
          | 0 -> (choose_int2 l >>= fun a -> 
                  ret_gen( a::acc))
          | n -> choose_int2 l >>= fun li ->
                 if li = maxi then
                   ret_gen (li::acc)
                 else
                   arbitrary_sorted (n-1) (li+1) (li::acc)

      let arbitrary = 
        choose_int(1,30) >>= fun n ->
        arbitrary_sorted n 0 [] >>= fun l ->
        ret_gen l
      
      let marshal = Hello.Element.VersionBitMap.marshal
      let parse = Hello.Element.VersionBitMap.parse
      let to_string = Hello.Element.VersionBitMap.to_string
      let size_of = Hello.Element.VersionBitMap.sizeof
    end
    
    type t = Hello.Element.t

    let arbitrary = 
      VersionBitMap.arbitrary >>= fun version ->
      ret_gen (VersionBitMap version)

    let marshal = Hello.Element.marshal
    let parse = Hello.Element.parse
    let to_string = Hello.Element.to_string
    let size_of = Hello.Element.sizeof
  end
  
  type t = Hello.t

  let arbitrary = 
    arbitrary_list Element.arbitrary >>= fun element ->
    ret_gen element

  let marshal = Hello.marshal
  let parse = Hello.parse
  let to_string = Hello.to_string
  let size_of = Hello.sizeof

end

module FlowRemoved = struct

  open Gen
  open OpenFlow0x04_Core

  type t = FlowRemoved.t

  let arbitrary_reason = 
    oneof [ 
      ret_gen FlowIdleTimeout;
      ret_gen FlowHardTiemout;
      ret_gen FlowDelete;
      ret_gen FlowGroupDelete]

  let arbitrary =
    arbitrary_uint64 >>= fun cookie ->
    arbitrary_uint16 >>= fun priority ->
    arbitrary_reason >>= fun reason ->
    arbitrary_uint8 >>= fun table_id ->
    arbitrary_uint32 >>= fun duration_sec ->
    arbitrary_uint32 >>= fun duration_nsec ->
    arbitrary_timeout >>= fun idle_timeout ->
    arbitrary_timeout >>= fun hard_timeout ->
    arbitrary_uint64 >>= fun packet_count ->
    arbitrary_uint64 >>= fun byte_count ->
    OfpMatch.arbitrary >>= fun oxm ->
    ret_gen { cookie; priority; reason; table_id; duration_sec; duration_nsec;
              idle_timeout; hard_timeout; packet_count; byte_count; oxm }

  let marshal = FlowRemoved.marshal
  let parse = FlowRemoved.parse
  let to_string = FlowRemoved.to_string
  let size_of = FlowRemoved.sizeof

end

module AsyncConfig = struct

  open Gen
  open OpenFlow0x04_Core

  type t = AsyncConfig.t

  let arbitrary_FlowReason = 
    oneof [ 
      ret_gen FlowIdleTimeout;
      ret_gen FlowHardTiemout;
      ret_gen FlowDelete;
      ret_gen FlowGroupDelete]

  let arbitrary_PacketInReason =
      oneof [
          ret_gen NoMatch;
          ret_gen ExplicitSend;
          ret_gen InvalidTTL
      ]

  let arbitrary_PortStatusReason =
        oneof [
            ret_gen PortAdd;
            ret_gen PortDelete;
            ret_gen PortModify
        ]

  let arbitrary_mask arb =
    arb >>= fun m_master ->
    arb >>= fun m_slave ->
    ret_gen { m_master; m_slave }

  let arbitrary = 
    arbitrary_mask arbitrary_PacketInReason >>= fun packet_in ->
    arbitrary_mask arbitrary_PortStatusReason >>= fun port_status ->
    arbitrary_mask arbitrary_FlowReason >>= fun flow_removed ->
    ret_gen { packet_in; port_status; flow_removed }

  let marshal = AsyncConfig.marshal
  let parse = AsyncConfig.parse
  let to_string = AsyncConfig.to_string
  let size_of = AsyncConfig.sizeof

end

module Error = struct

  open Gen
  open OpenFlow0x04_Core

  type t = Error.t

  let arbitrary_helloFailed =
    oneof [
      ret_gen HelloIncompatible;
      ret_gen HelloPermError
    ]

  let arbitrary_badRequest =
    oneof [
      ret_gen ReqBadVersion;
      ret_gen ReqBadType;
      ret_gen ReqBadMultipart;
      ret_gen ReqBadExp;
      ret_gen ReqBadExpType;
      ret_gen ReqPermError;
      ret_gen ReqBadLen;
      ret_gen ReqBufferEmpty;
      ret_gen ReqBufferUnknown;
      ret_gen ReqBadTableId;
      ret_gen ReqIsSlave;
      ret_gen ReqBadPort;
      ret_gen ReqBadPacket;
      ret_gen ReqMultipartBufOverflow
    ]

  let arbitrary_badAction = 
    oneof [
      ret_gen ActBadType;
      ret_gen ActBadLen;
      ret_gen ActBadExp;
      ret_gen ActBadExpType;
      ret_gen ActBadOutPort;
      ret_gen ActBadArg;
      ret_gen ActPermError;
      ret_gen ActTooMany;
      ret_gen ActBadQueue;
      ret_gen ActBadOutGroup;
      ret_gen ActMatchInconsistent;
      ret_gen ActUnsupportedOrder;
      ret_gen ActBadTag;
      ret_gen ActBadSetTyp;
      ret_gen ActBadSetLen;
      ret_gen ActBadSetArg
    ]

  let arbitrary_badInstruction =
    oneof [
      ret_gen InstUnknownInst;
      ret_gen InstBadTableId;
      ret_gen InstUnsupInst;
      ret_gen InstUnsupMeta;
      ret_gen InstUnsupMetaMask;
      ret_gen InstBadExp;
      ret_gen InstBadExpTyp;
      ret_gen InstBadLen;
      ret_gen InstPermError
    ]

  let arbitrary_badMatch = 
    oneof [
      ret_gen MatBadTyp;
      ret_gen MatBadLen;
      ret_gen MatBadTag;
      ret_gen MatBadDlAddrMask;
      ret_gen MatBadNwAddrMask;
      ret_gen MatBadWildcards;
      ret_gen MatBadField;
      ret_gen MatBadValue;
      ret_gen MatBadMask;
      ret_gen MatBadPrereq;
      ret_gen MatDupField;
      ret_gen MatPermError
    ]

  let arbitrary_flowModFailed =
    oneof [
      ret_gen FlUnknown;
      ret_gen FlTableFull;
      ret_gen FlBadTableId;
      ret_gen FlOverlap;
      ret_gen FlPermError;
      ret_gen FlBadTimeout;
      ret_gen FlBadCommand;
      ret_gen FlBadFlags
    ]

  let arbitrary_groupModFailed = 
    oneof [
      ret_gen GrGroupExists;
      ret_gen GrInvalidGroup;
      ret_gen GrWeightUnsupported;
      ret_gen GrOutOfGroups;
      ret_gen GrOutOfBuckets;
      ret_gen GrChainingUnsupported;
      ret_gen GrWatcHUnsupported;
      ret_gen GrLoop;
      ret_gen GrUnknownGroup;
      ret_gen GrChainedGroup;
      ret_gen GrBadTyp;
      ret_gen GrBadCommand;
      ret_gen GrBadBucket;
      ret_gen GrBadWatch;
      ret_gen GrPermError
    ]

  let arbitrary_portModFailed = 
    oneof [
      ret_gen PoBadPort;
      ret_gen PoBadHwAddr;
      ret_gen PoBadConfig;
      ret_gen PoBadAdvertise;
      ret_gen PoPermError
    ]

  let arbitrary_tableModFailed = 
    oneof [
      ret_gen TaBadTable;
      ret_gen TaBadConfig;
      ret_gen TaPermError
    ]

  let arbitrary_queueOpFailed =
    oneof [
      ret_gen QuBadPort;
      ret_gen QuBadQUeue;
      ret_gen QuPermError
    ]

  let arbitrary_switchConfigFailed =
    oneof [
      ret_gen ScBadFlags;
      ret_gen ScBadLen;
      ret_gen ScPermError
    ]

  let arbitrary_roleReqFailed =
    oneof [
      ret_gen RoStale;
      ret_gen RoUnsup;
      ret_gen RoBadRole;
    ]

  let arbitrary_meterModFailed =
    oneof [
      ret_gen MeUnknown;
      ret_gen MeMeterExists;
      ret_gen MeInvalidMeter;
      ret_gen MeUnknownMeter;
      ret_gen MeBadCommand;
      ret_gen MeBadFlags;
      ret_gen MeBadRate;
      ret_gen MeBadBurst;
      ret_gen MeBadBand;
      ret_gen MeBadBandValue;
      ret_gen MeOutOfMeters;
      ret_gen MeOutOfBands
    ]

  let arbitrary_tableFeatFailed = 
    oneof [
      ret_gen TfBadTable;
      ret_gen TfBadMeta;
      ret_gen TfBadType;
      ret_gen TfBadLen;
      ret_gen TfBadArg;
      ret_gen TfPermError
    ]

  let arbitrary_exp = 
    arbitrary_uint16 >>= fun exp_typ ->
    arbitrary_uint32 >>= fun exp_id -> 
    ret_gen {exp_typ; exp_id}

  let arbitrary_err = 
    oneof [
      arbitrary_helloFailed >>= (fun n -> ret_gen (HelloFailed n));
      arbitrary_badRequest >>= (fun n -> ret_gen (BadRequest n));
      arbitrary_badAction >>= (fun n -> ret_gen (BadAction n));
      arbitrary_badInstruction >>= (fun n -> ret_gen (BadInstruction n));
      arbitrary_badMatch >>= (fun n -> ret_gen (BadMatch n));
      arbitrary_flowModFailed >>= (fun n -> ret_gen (FlowModFailed n));
      arbitrary_groupModFailed >>= (fun n -> ret_gen (GroupModFailed n));
      arbitrary_portModFailed >>= (fun n -> ret_gen (PortModFailed n));
      arbitrary_tableModFailed >>= (fun n -> ret_gen (TableModFailed n));
      arbitrary_queueOpFailed >>= (fun n -> ret_gen (QueueOpFailed n));
      arbitrary_switchConfigFailed >>= (fun n -> ret_gen (SwitchConfigFailed n));
      arbitrary_roleReqFailed >>= (fun n -> ret_gen (RoleReqFailed n));
      arbitrary_meterModFailed >>= (fun n -> ret_gen (MeterModFailed n));
      arbitrary_tableFeatFailed >>= (fun n -> ret_gen (TableFeatFailed n));
      arbitrary_exp >>= (fun n -> ret_gen (ExperimenterFailed n));
    ]

  let arbitrary_len =
      (choose_int (64, 150)) >>= fun a ->
       ret_gen a

  let arbitrary_byte n =
  (* construct an arbitrary byte of length n*)
      arbitrary_stringN n  >>= fun a ->
      let byte = Cstruct.create n in
      Cstruct.blit_from_string a 0 byte 0 n;
      ret_gen (byte)
      
  let arbitrary = 
    arbitrary_len >>= fun len ->
    arbitrary_byte len >>= fun data ->
    arbitrary_err >>= fun err ->
    ret_gen {
      Error.err = err;
      Error.data = data}

  let marshal = Error.marshal
  let parse = Error.parse
  let to_string = Error.to_string
  let size_of = Error.sizeof

end

