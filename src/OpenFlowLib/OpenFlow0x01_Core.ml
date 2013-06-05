open Packet

type switchId = int64

type portId = int16

type xid = int32

type pattern =  
    { dlSrc : dlAddr option
    ; dlDst : dlAddr option
    ; dlTyp : dlTyp option
    ; dlVlan : dlVlan option
    ; dlVlanPcp : dlVlanPcp option
    ; nwSrc : nwAddr option
    ; nwDst : nwAddr option
    ; nwProto : nwProto option
    ; nwTos : nwTos option
    ; tpSrc : tpPort option
    ; tpDst : tpPort option
    ; inPort : portId option
    }


type pseudoPort =
  | PhysicalPort of portId
  | AllPorts
  | InPort
  | Flood
  | Controller of int
