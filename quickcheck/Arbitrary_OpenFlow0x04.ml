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

    module OxmHeader = struct
        type t = OpenFlow0x04_Core.oxm
        
        module Oxm = OpenFlow0x04.Oxm
        
        let arbitrary = 
            let open Gen in
            let open Oxm in
            arbitrary_masked (ret_gen 0L) (ret_gen 0L) >>= fun oxmMetadata ->
            arbitrary_masked (ret_gen 0L) (ret_gen 0L) >>= fun oxmEthDst ->
            arbitrary_masked (ret_gen 0L) (ret_gen 0L) >>= fun oxmEthSrc ->
            arbitrary_masked (ret_gen 0) (ret_gen 0) >>= fun oxmVlanVId ->
            arbitrary_masked (ret_gen 0l) (ret_gen 0l) >>= fun oxmIP4Src ->
            arbitrary_masked (ret_gen 0l) (ret_gen 0l) >>= fun oxmIP4Dst ->
            arbitrary_masked (ret_gen 0) (ret_gen 0) >>= fun oxmTCPSrc ->
            arbitrary_masked (ret_gen 0) (ret_gen 0) >>= fun oxmTCPDst ->
            arbitrary_masked (ret_gen 0l) (ret_gen 0l) >>= fun oxmARPSpa ->
            arbitrary_masked (ret_gen 0l) (ret_gen 0l) >>= fun oxmARPTpa ->
            arbitrary_masked (ret_gen 0L) (ret_gen 0L) >>= fun oxmARPSha ->
            arbitrary_masked (ret_gen 0L) (ret_gen 0L) >>= fun oxmARPTha ->
            arbitrary_masked (ret_gen 0L) (ret_gen 0L) >>= fun oxmTunnelId ->
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
                ret_gen (OxmTCPSrc oxmTCPSrc);
                ret_gen (OxmTCPDst oxmTCPDst);
                ret_gen (OxmARPOp 0);
                ret_gen (OxmARPSpa oxmARPSpa);
                ret_gen (OxmARPTpa oxmARPTpa);
                ret_gen (OxmARPSha oxmARPSha);
                ret_gen (OxmARPTha oxmARPTha);
                ret_gen (OxmICMPType 0);
                ret_gen (OxmICMPCode 0);
                ret_gen (OxmMPLSLabel 0l);
                ret_gen (OxmMPLSTc 0);
                ret_gen (OxmTunnelId oxmTunnelId)
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

module MultipartReq = struct
  open Gen
  open OpenFlow0x04_Core
  module TableFeaturesRequest = struct
    module TableFeatureProp = struct
      
      type t = OpenFlow0x04_Core.tableFeatureProp
      
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
      type t = OpenFlow0x04_Core.tableFeatures

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

    type t = OpenFlow0x04_Core.tableFeaturesRequest

    let arbitrary =
        list1 TableFeature.arbitrary >>= fun v ->
        ret_gen v
    let marshal = TableFeaturesRequest.marshal
    let parse = TableFeaturesRequest.parse
    let to_string = TableFeaturesRequest.to_string
    let size_of = TableFeaturesRequest.sizeof
  end

  module FlowRequest = struct
    type t = OpenFlow0x04_Core.flowRequest
    
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
    type t = OpenFlow0x04_Core.queueRequest

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
  
  type t = OpenFlow0x04_Core.multipartRequest
  
  let arbitrary_option =
     frequency [
    (1, ret_gen None);
    (3, TableFeaturesRequest.arbitrary >>= (fun v -> ret_gen (Some v)))
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
  module PortsDescriptionReply = struct
    type t = OpenFlow0x04_Core.portDesc list
    
    let arbitrary =
        list1 PortDesc.arbitrary >>= fun v ->
        ret_gen v
    
    let marshal = PortsDescriptionReply.marshal
    let parse = PortsDescriptionReply.parse
    let to_string = PortsDescriptionReply.to_string
    let size_of = PortsDescriptionReply.sizeof
  end
  
  module FlowStats = struct
    type t = OpenFlow0x04_Core.flowStats list

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

    let arbitrary_flow =
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
    
    let arbitrary =
        list1 arbitrary_flow >>= fun v ->
        ret_gen v
    
    let marshal = FlowStats.marshal
    let parse = FlowStats.parse
    let to_string = FlowStats.to_string
    let size_of = FlowStats.sizeof
  end
  
  module AggregateStats = struct
    type t = OpenFlow0x04_Core.aggregStats
    
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
        type t = OpenFlow0x04_Core.tableStats list
    
    let arbitrary_table =
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
    
    let arbitrary = 
        list1 arbitrary_table >>= fun v ->
        ret_gen v
    
    let marshal = TableStats.marshal
    let parse = TableStats.parse
    let to_string = TableStats.to_string
    let size_of = TableStats.sizeof
  end

  module PortStats = struct
        type t = OpenFlow0x04_Core.portStats list
    
    let arbitrary_portStats =
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
    
    let arbitrary = 
        list1 arbitrary_portStats >>= fun v ->
        ret_gen v
    
    let marshal = PortStats.marshal
    let parse = PortStats.parse
    let to_string = PortStats.to_string
    let size_of = PortStats.sizeof
  end

  module SwitchDescriptionReply = struct
    type t = OpenFlow0x04_Core.switchDesc
    
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

    type t = OpenFlow0x04_Core.queueStats list

    let arbitrary_queueStats =
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

    let arbitrary =
        list1 arbitrary_queueStats >>= fun v ->
        ret_gen v

    let marshal = QueueStats.marshal
    let parse = QueueStats.parse
    let to_string = QueueStats.to_string
    let size_of = QueueStats.sizeof


  end

  module GroupStats = struct

    module BucketStats = struct

        type t = OpenFlow0x04_Core.bucketStats list

        let arbitrary_bucketStats =
            arbitrary_uint64 >>= fun packet_count ->
            arbitrary_uint64 >>= fun byte_count ->
            ret_gen {packet_count; byte_count}

        let arbitrary =
            list1 arbitrary_bucketStats >>= fun v ->
            ret_gen v

        let marshal = GroupStats.BucketStats.marshal
        let parse = GroupStats.BucketStats.parse
        let to_string = GroupStats.BucketStats.to_string
        let size_of = GroupStats.BucketStats.sizeof
    end
  
    type t = OpenFlow0x04_Core.groupStats list

    let calc_length bs =
        (* sizeof_ofp_group_stats = 40*)
        ret_gen (40+(BucketStats.size_of bs))

    let arbitrary_groupStats =
        arbitrary_uint32 >>= fun group_id ->
        arbitrary_uint32 >>= fun ref_count ->
        arbitrary_uint64 >>= fun packet_count ->
        arbitrary_uint64 >>= fun byte_count ->
        arbitrary_uint32 >>= fun duration_sec ->
        arbitrary_uint32 >>= fun duration_nsec ->
        BucketStats.arbitrary >>= fun bucket_stats ->
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

    let arbitrary =
        list1 arbitrary_groupStats >>= fun v ->
        ret_gen v

    let marshal = GroupStats.marshal
    let parse = GroupStats.parse
    let to_string = GroupStats.to_string
    let size_of = GroupStats.sizeof
  end

  type t = OpenFlow0x04_Core.multipartReply

  let arbitrary =
      arbitrary_bool >>= fun flags ->
      oneof [
          PortsDescriptionReply.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (PortsDescReply n); mpreply_flags = flags});
          SwitchDescriptionReply.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (SwitchDescReply n); mpreply_flags = flags});
          FlowStats.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (FlowStatsReply n); mpreply_flags = flags});
          AggregateStats.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (AggregateReply n); mpreply_flags = flags});
          TableStats.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (TableReply n); mpreply_flags = flags});
          PortStats.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (PortStatsReply n); mpreply_flags = flags});
          QueueStats.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (QueueStatsReply n);  mpreply_flags = flags});
          GroupStats.arbitrary >>= (fun n -> ret_gen {mpreply_typ = (GroupStatsReply n);  mpreply_flags = flags});
          ]

  let marshal = MultipartReply.marshal
  let parse = MultipartReply.parse
  let to_string = MultipartReply.to_string
  let size_of = MultipartReply.sizeof
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

module RoleRequest = struct
  open Gen
  open OpenFlow0x04_Core

  type t = OpenFlow0x04_Core.roleRequest

  let arbitrary_role = 
    oneof [
      ret_gen NoChangeRole;
      ret_gen EqualRole;
      ret_gen MasterRole;
      ret_gen SlaveRole
    ]

  let arbitrary = 
    arbitrary_role >>= fun role ->
    arbitrary_uint64 >>= fun generation_id -> 
    ret_gen { role; generation_id }

  let marshal = RoleRequest.marshal
  let parse = RoleRequest.parse
  let to_string = RoleRequest.to_string
  let size_of = RoleRequest.sizeof
end
