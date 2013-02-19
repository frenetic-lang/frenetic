(** Types for OpenFlow 1.3.1, based on
    
    https://www.opennetworking.org/images/stories/downloads/specification/openflow-spec-v1.3.1.pdf

    Using this as a guide:

    https://github.com/CPqD/ofsoftswitch13/blob/master/include/openflow/openflow.h

*)

open Cstruct

type uint48 = uint64
type uint12 = uint16

type +'a mask = {
  bits : 'a;
  mask : 'a
}

(** See Table 11 of the specification *)
type oxm = 
  | OxmInPort of uint32
  | OxmInPhyPort of uint32
  | OxmMetadata of uint64 mask
  | OxmEthType of uint16
  | OxmEthDst of uint48 mask
  | OxmEthSrc of uint48 mask
  | OxmVlanVId of uint12 mask * bool mask


(**  Hard-codes OFPMT_OXM as the match type, since OFPMT_STANDARD is deprecated.
*)
type oxmMatch = oxm list
