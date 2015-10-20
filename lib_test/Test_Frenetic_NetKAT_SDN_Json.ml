open OUnitHack
open Core.Std
open Frenetic_OpenFlow
open Frenetic_NetKAT_SDN_Json
open Yojson.Basic

TEST "pseudoport_to_json serializes a physical port in json format" = 
  let phys_port = Physical 6325l in
  to_string (pseudoport_to_json phys_port) = "{\"type\":\"physical\",\"port\":6325}"

TEST "pseudoport_to_json serializes a controller port in json format" = 
  let controller_port = Controller 5236 in
  to_string (pseudoport_to_json controller_port) = "{\"type\":\"controller\",\"bytes\":5236}"

TEST "pseudoport_from_json parses a physical port from json format" = 
  let phys_port_json_str = from_string "{\"type\":\"physical\",\"port\":6325}" in
  (pseudoport_from_json phys_port_json_str) = Physical 6325l

TEST "pseudoport_to_json serializes a controller port in json format" = 
  let controller_port_json_str = from_string "{\"type\":\"controller\",\"bytes\":5236}" in
  (pseudoport_from_json controller_port_json_str) = Controller 5236

TEST "pkt_out returns switch and abstract Packet Out message from Json format" =
  let sample_packet_out_str = In_channel.read_all "lib_test/data/pkt_out_multiple_ports.json" in
  let sample_packet_out = pkt_out_from_json (from_string sample_packet_out_str) in
  sample_packet_out = ( 
    843509872345L, 
    (NotBuffered (Cstruct.of_string "Hi mom!"), Some 20l, [ Output(Physical 1l); Output(Physical 2l) ]) 
  )

TEST "pkt_out handles buffered data Json format" =
  let sample_packet_out_str = In_channel.read_all "lib_test/data/pkt_out_buffered.json" in
  let sample_packet_out = pkt_out_from_json (from_string sample_packet_out_str) in
  sample_packet_out = ( 
    843509872345L, 
    (Buffered (8192374l, Cstruct.of_string ""), None, [ Output(Controller 1024) ]) 
  )

TEST "pkt_out doesn't support modify action" =
  let sample_packet_out_modify_json = from_string "{ \"actions\": [ {\"type\": \"modify\"} ] }" in
  try 
    (pkt_out_from_json sample_packet_out_modify_json) = (0L, (NotBuffered (Cstruct.of_string ""), None, []))
  with Failure _ -> true | _ -> false 

TEST "flowTable_to_json serializes a flow table in Json format" = 
  let ft = Test_Frenetic_OpenFlow.nightmare_pattern_table in
  to_string (flowTable_to_json ft) = (In_channel.read_all "lib_test/data/flow_table_nightmare_pattern.json") 

TEST "flowTable_to_json doesn't handle multiple actions" = 
  let ft = Test_Frenetic_OpenFlow.sample_flow_table in
  try 
    to_string (flowTable_to_json ft) = ""
  with Failure _ -> true | _ -> false 
   