open Datatypes
open List0
open WordInterface
open OpenFlow0x04Types
open Packet
open MessagesDef
open NetCoreEval

type modification = NetCoreEval.modification
val modifyDlSrc : modification -> dlAddr option
val modifyDlDst : modification -> dlAddr option
val modifyDlVlan : modification -> dlVlan option option
val modifyDlVlanPcp : modification -> dlVlanPcp option
val modifyNwSrc : modification -> nwAddr option
val modifyNwDst : modification -> nwAddr option
val modifyNwTos : modification -> nwTos option
val modifyTpSrc : modification -> tpPort option
val modifyTpDst : modification -> tpPort option
val unmodified : modification

type act =
| Forward of modification * pseudoPort
| Group of groupId
| ActGetPkt of id

type pol =
| PoAtom of pred * act list
| PoUnion of pol * pol

val convert_from_eval10 : NetCoreEval.pol -> pol
