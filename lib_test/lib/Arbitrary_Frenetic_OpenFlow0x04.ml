open Frenetic_OpenFlow0x04

let sample_single_action =
  [ Output(PhysicalPort(666l)) ]

let sample_lotsa_actions = [
  Output(PhysicalPort(983745l));
  Output(InPort);
  Output(Table);
  Output(Normal);
  Output(Flood);
  Output(AllPorts);
  Output(Controller(6));
  Output(Local);
  Output(Any);
  Group(9l);
  SetQueue(10l);
  SetMplsTtl(11);
  DecMplsTtl;
  SetNwTtl(13);
  DecNwTtl;
  CopyTtlOut;
  CopyTtlIn;
  PushVlan(17); 
  PushMpls(18); 
  PopVlan;
  PopMpls(20);
  SetField(OxmEthSrc({ m_value = 21L; m_mask = None}))
]

let sample_pipeline_match = [
  OxmInPort 1l;
  OxmInPhyPort 2l;
  OxmMetadata { m_value = 3L; m_mask = Some 0xffL };
  OxmTunnelId { m_value = 4L; m_mask = None }
]

let sample_lotsa_matches = [
  OxmInPort 1l;
  OxmInPhyPort 2l;
  OxmMetadata { m_value = 3L; m_mask = Some 0xffL };
  OxmEthDst { m_value = 4L; m_mask = None };
  OxmEthSrc { m_value = 5L; m_mask = None };
  OxmEthType 6;
  OxmVlanVId { m_value = 7; m_mask = None }; 
  OxmVlanPcp 8; 
  OxmIPDscp 9; 
  OxmIPEcn 10; 
  OxmIPProto 11;
  OxmIP4Src { m_value = 12l; m_mask = None }; 
  OxmIP4Dst { m_value = 13l; m_mask = None }; 
  OxmTCPSrc 14;
  OxmTCPDst 15; 
  OxmUDPSrc 16;
  OxmUDPDst 17;
  OxmSCTPSrc 18;
  OxmSCTPDst 19;
  OxmICMPType 20;
  OxmICMPCode 21; 
  OxmARPOp 22;
  OxmARPSpa { m_value = 23l; m_mask = None }; 
  OxmARPTpa { m_value = 24l; m_mask = None }; 
  OxmARPSha { m_value = 25L; m_mask = None };
  OxmARPTha { m_value = 26L; m_mask = None };
  OxmIPv6Src { m_value = (0L,27L); m_mask = None };
  OxmIPv6Dst { m_value = (0L,28L); m_mask = None };
  OxmIPv6FLabel { m_value = 29l; m_mask = None };
  OxmICMPv6Type 30; 
  OxmICMPv6Code 31;
  OxmIPv6NDTarget { m_value = (0L,32L); m_mask = None };
  OxmIPv6NDSll 33L;
  OxmIPv6NDTll 34L;
  OxmMPLSLabel 35l;
  OxmMPLSTc 36;
  OxmMPLSBos true; 
  OxmPBBIsid { m_value = 38l; m_mask = None }; 
  OxmTunnelId { m_value = 39L; m_mask = None };
  OxmIPv6ExtHdr { m_value = { noext = false; esp = false; auth = false;
                       dest = true; frac = false; router = true;
                       hop = false; unrep = false; unseq = false }; m_mask = None };
]
