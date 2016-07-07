open Core.Std
open Frenetic_Fdd
open Frenetic_Network
open Frenetic_OpenFlow
open Frenetic_NetKAT

module FieldTable : Hashtbl.S with type key = Field.t

exception IncompletePlace of string
exception NonExistentPath of string
exception NonFilterNode of policy
exception ClashException of string
exception CorrelationException of string

type condition = (Value.t option * Value.t list) FieldTable.t
type place     = (switchId * portId)
type path      = pred * place list
type stream    = place * place * condition * Action.t

val dedup : policy -> policy
val streams_of_policy : policy -> stream list
val string_of_stream : stream -> string
val paths_of_string  : string -> (path list, string) Result.t
val assemble : policy -> policy ->
  (switchId * portId) list -> (switchId * portId) list ->
  policy
val retarget : stream list -> stream list -> policy ->
  (policy list * policy list)
val project : path list -> stream list -> policy ->
  (policy list * policy list)
