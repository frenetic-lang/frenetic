open Datatypes
open List0
open OpenFlow0x04Types
open MessagesDef
open Packet
open WordInterface
open NetCoreEval

type modification = NetCoreEval.modification
let modifyTpDst = NetCoreEval.modifyTpDst
let modifyTpSrc = NetCoreEval.modifyTpSrc
let modifyNwTos = NetCoreEval.modifyNwTos
let modifyNwDst = NetCoreEval.modifyNwDst
let modifyNwSrc = NetCoreEval.modifyNwSrc
let modifyDlVlanPcp = NetCoreEval.modifyDlVlanPcp
let modifyDlVlan = NetCoreEval.modifyDlVlan
let modifyDlDst = NetCoreEval.modifyDlDst
let modifyDlSrc = NetCoreEval.modifyDlSrc
let modification_rec = NetCoreEval.modification_rec
let modification_rect = NetCoreEval.modification_rect
let unmodified = NetCoreEval.unmodified

type act =
| Forward of modification * pseudoPort
| Group of groupId
| ActGetPkt of id

type pol =
| PoAtom of pred * act list
| PoUnion of pol * pol

let eval_to_eval13 act = match act with
  | NetCoreEval.Forward (a,b) -> Forward (a,b)
  | NetCoreEval.ActGetPkt a -> ActGetPkt a

let rec convert_from_eval10 pol = match pol with 
  | NetCoreEval.PoAtom (pred,acts) -> 
    PoAtom (pred, List.map eval_to_eval13 acts)
  | NetCoreEval.PoUnion(pol1,pol2) -> 
    PoUnion(convert_from_eval10 pol1, convert_from_eval10 pol2)
