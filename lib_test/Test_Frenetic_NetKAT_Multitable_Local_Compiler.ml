open OUnitHack
open Core.Std
open Frenetic_OpenFlow
open Frenetic_NetKAT
open Frenetic_NetKAT_Pretty
open Frenetic_NetKAT_Local_Compiler
open Frenetic_NetKAT_Local_Compiler.Field
    
TEST "Multitable, each table matches on one field" =
  let layout = [[EthDst];[EthType];[EthSrc];[Location]] in
  let pol = Frenetic_NetKAT_Parser.policy_from_string 
    "(filter ethDst = 1; port := 2) | (filter ethTyp = 0x806; port := 1) | 
     (filter ethSrc = 2; port := 1) | (filter port = 2; port := 12)" in
  let order = `Static [EthDst; EthType; EthSrc; Location; IPProto; Switch; 
                       Vlan; VlanPcp; IP4Src; IP4Dst; TCPSrcPort; TCPDstPort] in
  let fdd = compile pol ~order:order in
  let subtrees = flow_table_subtrees layout fdd in
  print_string (Frenetic_NetKAT_Local_Compiler.to_string fdd);
  Map.to_alist subtrees = []

TEST "Multitable, match multiple fields per table" =
  let layout = [[EthDst;EthType];[EthSrc;Location]] in
  let pol = Frenetic_NetKAT_Parser.policy_from_string
    "(filter ethDst = 1; port := 2) | (filter ethTyp = 0x806; port := 1) | 
     (filter ethSrc = 2; port := 1) | (filter port = 2; port := 12)" in
  let order = `Static [EthDst; EthType; EthSrc; Location; IPProto; Switch; 
                       Vlan; VlanPcp; IP4Src; IP4Dst; TCPSrcPort; TCPDstPort] in
  let fdd = compile pol ~order:order in
  let subtrees = flow_table_subtrees layout fdd in
  Map.to_alist subtrees = []
