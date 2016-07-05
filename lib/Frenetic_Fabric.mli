open Core.Std
open Frenetic_Fdd
open Frenetic_Network
open Frenetic_OpenFlow
open Frenetic_NetKAT

module FieldTable : Hashtbl.S with type key = Field.t

type condition = (Value.t option * Value.t list) FieldTable.t
type place     = (switchId * portId)
type stream    = place * place * condition * Action.t

(* val conds_of_pred : pred -> condition list list *)
(* val pred_of_conds : condition list -> pred *)

val dedup : policy -> policy
val streams_of_policy : policy -> stream list
val assemble : policy -> policy ->
  (switchId * portId) list -> (switchId * portId) list ->
  policy
val string_of_stream : stream -> string
val retarget : stream list -> stream list -> policy ->
  (policy list * policy list)

