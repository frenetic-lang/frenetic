open Core
open Yojson.Basic
open Frenetic_NetKAT_Json

(* TODO: Right now, NetKAT_Parser does not do optimization, but NetKAT_Json does, making it tough to do
   more extensive testing.  When that's worked out, add more tests here. *)

let%test "pol_of_json deserializes a NetKAT-Json from example1 into a NetKAT policy" =
  let nk_policy = Frenetic_NetKAT_Parser.pol_of_string (In_channel.read_all "examples/example1.kat") in
  let example1_json = from_string (In_channel.read_all "lib_test/data/example1_reversed.json") in
  pol_of_json example1_json = nk_policy

let%test "pol_of_json deserializes a NetKAT-Json from fall-through-optimization into a NetKAT policy" =
  (* Note: we can't just read the policy from fall-through-optimization.kat like we did in the last test case
    because pol_of_json does optimizations that pol_of_string doesn't do.
  *)
  let fall_json = from_string (In_channel.read_all "lib_test/data/fall-through-optimization.json") in
  pol_of_json fall_json = Filter ( Test_Frenetic_NetKAT_Parser.and3 (Test(Vlan(1))) (Test(VlanPcp(1))) (Test(TCPSrcPort(1))) ) 

let%test "policy_to_json serializes a NetKAT policy like example1.kat" = 
  let nk_policy = Frenetic_NetKAT_Parser.pol_of_string (In_channel.read_all "examples/example1.kat") in
  let example1_json = from_string (In_channel.read_all "lib_test/data/example1.json") in
  (to_string example1_json) = (to_string (policy_to_json nk_policy))

let%test "event_to_json_string serializes an OpenFlow Packet In event with a format like pkt_in.json" = 
  let pktin_event = Frenetic_OpenFlow.PacketIn(
    "learning_switch", 
    0x871234L, 
    99l, 
    Frenetic_OpenFlow.Buffered(0l, Cstruct.of_string "Hi mom!"), 
    1024,
    ExplicitSend
  ) in
  let pkt_in_json = In_channel.read_all "lib_test/data/pkt_in.json" in
  event_to_json_string pktin_event = pkt_in_json 

let%test "event_to_json_string serializes an OpenFlow Switch Up event with a format like switch_up.json" = 
  let switch_up_event = Frenetic_OpenFlow.SwitchUp(
    0x9878abcL, 
    [1l; 2l; 38l]
  ) in
  let switch_up_json = In_channel.read_all "lib_test/data/switch_up.json" in
  event_to_json_string switch_up_event = switch_up_json 

let%test "event_to_json_string serializes an OpenFlow Switch Down event with a format like switch_down.json" = 
  let switch_down_event = Frenetic_OpenFlow.SwitchDown(
    0x9878abcL
  ) in
  let switch_down_json = In_channel.read_all "lib_test/data/switch_down.json" in
  event_to_json_string switch_down_event = switch_down_json 

let%test "event_to_json_string serializes an OpenFlow Port Up event with a format like port_up.json" = 
  let port_up_event = Frenetic_OpenFlow.PortUp(
    0x9878abcL,
    1l
  ) in
  let port_up_json = In_channel.read_all "lib_test/data/port_up.json" in
  event_to_json_string port_up_event = port_up_json 

let%test "event_to_json_string serializes an OpenFlow Port Down event with a format like port_down.json" = 
  let port_down_event = Frenetic_OpenFlow.PortDown(
    0x9878abdL,
    6l
  ) in
  let port_down_json = In_channel.read_all "lib_test/data/port_down.json" in
  event_to_json_string port_down_event = port_down_json 

let%test "stats_to_json_string serializes an OpenFlow stats response like stats.json" = 
  let stats_response = (0xaceL, 0xace45L) in
  let stats_json = In_channel.read_all "lib_test/data/stats.json" in
  stats_to_json_string stats_response = stats_json 

let sample_port_stats_response = 
  let open Frenetic_OpenFlow in 
  { 
    port_no = 1000L
    ; port_rx_packets = 2000000000L
    ; port_tx_packets = 3000000000L
    ; port_rx_bytes = 4000000000L
    ; port_tx_bytes = 5000000000L
    ; port_rx_dropped = 6000000000L
    ; port_tx_dropped = 7000000000L
    ; port_rx_errors = 8000000000L
    ; port_tx_errors = 9000000000L
    ; port_rx_frame_err = 10000000000L
    ; port_rx_over_err = 11000000000L
    ; port_rx_crc_err = 12000000000L
    ; port_collisions = 13000000000L
  }

let sample_flow_stats_response =
  let open Frenetic_OpenFlow in {
    flow_table_id = 66L; flow_pattern = Pattern.match_all;
    flow_actions = []; flow_duration_sec = 200L; flow_duration_nsec = 300L;
    flow_priority = 400L; flow_idle_timeout = 500L; flow_hard_timeout = 600L;
    flow_packet_count = 700L; flow_byte_count = 800L
  }  

let%test "port_stats_to_json serializes an OpenFlow stats response like port_stats.json" = 
  let port_stats_json = In_channel.read_all "lib_test/data/port_stats.json" in
  port_stat_to_json_string sample_port_stats_response = port_stats_json 

let%test "pol_of_json_string deserializes a NetKAT-Json string from example1 into a NetKAT policy" =
  let nk_policy = Frenetic_NetKAT_Parser.pol_of_string (In_channel.read_all "examples/example1.kat") in
  let example1_json_str = In_channel.read_all "lib_test/data/example1_reversed.json" in
  pol_of_json_string example1_json_str = nk_policy

let%test "event_to_json_string serializes an OpenFlow FlowStats event with a format like flow_stats.json" = 
  let query_event = Frenetic_OpenFlow.FlowStats(
    0x9878abcL, 
    sample_flow_stats_response
  ) in
  let query_json_str = In_channel.read_all "lib_test/data/flow_stats.json" in
  event_to_json_string query_event = query_json_str

let%test "policy_to_json_string serializes a NetKAT policy like example1.kat" = 
  let nk_policy = Frenetic_NetKAT_Parser.pol_of_string (In_channel.read_all "examples/example1.kat") in
  let example1_json_str = In_channel.read_all "lib_test/data/example1.json" in
  example1_json_str = policy_to_json_string nk_policy

let%test "stats_to_json_string serializes an OpenFlow stats response like stats.json" = 
  let stats_response = (0xaceL, 0xace45L) in
  let stats_json_str = In_channel.read_all "lib_test/data/stats.json" in
  stats_to_json_string stats_response = stats_json_str 

let%test "port_stats_to_json_string serializes an OpenFlow stats response like port_stats.json" = 
  let port_stats_json_str = In_channel.read_all "lib_test/data/port_stats.json" in
  port_stat_to_json_string sample_port_stats_response = port_stats_json_str


let%test "pseudoport_to_json serializes a physical port in json format" = 
  let phys_port = Frenetic_OpenFlow.Physical 6325l in
  to_string (pseudoport_to_json phys_port) = "{\"type\":\"physical\",\"port\":6325}"

let%test "pseudoport_to_json serializes a controller port in json format" = 
  let controller_port = Frenetic_OpenFlow.Controller 5236 in
  to_string (pseudoport_to_json controller_port) = "{\"type\":\"controller\",\"bytes\":5236}"

let%test "pseudoport_from_json parses a physical port from json format" = 
  let phys_port_json_str = from_string "{\"type\":\"physical\",\"port\":6325}" in
  (pseudoport_from_json phys_port_json_str) = Physical 6325l

let%test "pseudoport_to_json serializes a controller port in json format" = 
  let controller_port_json_str = from_string "{\"type\":\"controller\",\"bytes\":5236}" in
  (pseudoport_from_json controller_port_json_str) = Controller 5236

let%test "pkt_out returns switch and abstract Packet Out message from Json format" =
  let sample_packet_out_str = In_channel.read_all "lib_test/data/pkt_out_multiple_ports.json" in
  let sample_packet_out = pkt_out_from_json (from_string sample_packet_out_str) in
  sample_packet_out = ( 
    843509872345L, 
    Some 20l,
    (NotBuffered (Cstruct.of_string "Hi mom!")), 
    [ Mod(Location(Physical 1l)); Mod(Location(Physical 2l)) ]
  )

let%test "pkt_out handles buffered data Json format" =
  let sample_packet_out_str = In_channel.read_all "lib_test/data/pkt_out_buffered.json" in
  let sample_packet_out = pkt_out_from_json (from_string sample_packet_out_str) in
  sample_packet_out = ( 
    843509872345L, 
    None, 
    Buffered (8192374l, Cstruct.of_string ""), 
    [ Mod(Location(Pipe "pipe")) ] 
  )

let%test "flowTable_to_json serializes a flow table in Json format" = 
  let ft = Test_Frenetic_OpenFlow.nightmare_pattern_table in
  to_string (flowTable_to_json ft) = (In_channel.read_all "lib_test/data/flow_table_nightmare_pattern.json") 

let%test "flowTable_to_json doesn't handle multiple actions" = 
  let ft = Test_Frenetic_OpenFlow.sample_flow_table in
  Exn.does_raise (fun () ->
    to_string (flowTable_to_json ft) = ""
  )
   
