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
        type t = OpenFlow0x04_Core.oxm
        
        let arbitrary = 
            let open Gen in
            let open Oxm in
            let arbitrary_dscp = 
              (choose_int (0,64)) >>= fun a ->
              ret_gen a in
            let arbitrary_ecn = 
            (choose_int (0,3)) >>= fun a ->
              ret_gen a in
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
                ret_gen (OxmTunnelId oxmTunnelId)
            ]
        let marshal = Oxm.marshal
        let to_string = Oxm.to_string
        let size_of = Oxm.sizeof
        let parse bits = 
            let p,_ = Oxm.parse bits in
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

    let arbitrary_timeout =
        let open OpenFlow0x04_Core in
        oneof [
            ret_gen Permanent;
            arbitrary_uint16 >>= (fun n -> ret_gen (ExpiresAfter n))
        ]

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
        PseudoPort.arbitrary >>= fun mfPort ->
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
