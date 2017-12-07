open! Core
open Probnetkat
open Probnetkat.Syntax
open Probnetkat.Symbolic
open Frenetic.Network

(** raw data from experiment *)
type data = {
  topology : string;
  routing_schme : string;
  max_failures : int; (* -1 means no limit *)
  failure_prob : float;
  equivalent_to_teleport : bool;
  min_prob_of_delivery : float;
  avg_prob_of_delivery : float;

  (* various times *)
  compilation_time : float;
} [@@deriving yojson]


(* module type Params : sig
  include module type of Probnetkat.Params in
  

let analyze ~topology ~routing_schme ~max_failures ~failure_prob =
  failwith "not implemented"

let analyze_model (model : module Model) =
 *)
