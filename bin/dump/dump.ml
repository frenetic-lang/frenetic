open! Core
open Probnetkat
open Probnetkat.Syntax
open Probnetkat.Symbolic
open Frenetic.Network

type data = {
  name : string;
  max_failures : int; (* -1 means no limit *)
  failure_prob : float;
  equivalent_to_teleport : bool;
  min_prob_of_delivery : float;
  avg_prob_of_delivery : float;
} [@@deriving yojson]
