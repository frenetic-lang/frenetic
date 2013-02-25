open Datatypes
open List0
open Packet
open WordInterface
open OpenFlow0x04Types
open MessagesDef
open NetCoreEval

type act =
| Forward of modification * pseudoPort
| Group of groupId
| ActGetPkt of id

type pol =
| PoAtom of pred * act list
| PoUnion of pol * pol

val eval_action : input -> act -> output

val classify : pol -> input -> output list

