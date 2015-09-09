open Frenetic_OpenFlow0x04

let sample_single_action =
  [ Output(PhysicalPort(666l)) ]

let sample_lotsa_actions = 
  [ 
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
