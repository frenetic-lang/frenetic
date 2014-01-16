open NetKAT_Types
type lf_policy

type port_dst

module SwitchMap : Map.S
  with type key = VInt.t

module PortMap : Map.S
  with type key = VInt.t

type portmap = port_dst PortMap.t
type topology = portmap SwitchMap.t

exception Inconsistent_topology

val lf_policy_to_policy : lf_policy -> policy
val topology_to_policy : topology -> policy

type 'a aregex =
  | Char of 'a
  | Pick of 'a aregex * 'a aregex
  | Alt of 'a aregex * 'a aregex
  | Cat of 'a aregex * 'a aregex
  | Kleene of 'a aregex
  | Empty

type pchar = lf_policy * topology
type regex = pchar aregex

val regex_of_policy : policy -> regex
val regex_to_policy : regex -> policy

val regex_to_string : regex -> string
val lf_policy_to_string : lf_policy -> string

val regex_to_aregex : regex -> int aregex * ((int, pchar) Hashtbl.t)
val regex_of_aregex : int aregex -> (int, pchar) Hashtbl.t -> regex

type 'a dehopified = 'a * ('a SwitchMap.t) * topology * 'a

val regex_to_switch_policies : regex -> policy dehopified
val dehopify : policy -> policy dehopified
