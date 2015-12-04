open Core.Std
open Yojson.Basic
open Frenetic_NetKAT_Json

(* TODO: Right now, NetKAT_Parser does not do optimization, but NetKAT_Json does, making it tough to do
   more extensive testing.  When that's worked out, add more tests here. *)

TEST "policy_from_json deserializes a NetKAT-Json from example1 into a NetKAT policy" =
  let nk_policy = Frenetic_NetKAT_Parser.policy_from_string (In_channel.read_all "examples/example1.kat") in
  let example1_json = from_string (In_channel.read_all "lib_test/data/example1_reversed.json") in
  policy_from_json example1_json = nk_policy

TEST "policy_from_json deserializes a NetKAT-Json from fall-through-optimization into a NetKAT policy" =
  (* Note: we can't just read the policy from fall-through-optimization.kat like we did in the last test case
    because policy_from_json does optimizations that policy_from_string doesn't do.
  *)
  let fall_json = from_string (In_channel.read_all "lib_test/data/fall-through-optimization.json") in
  policy_from_json fall_json = Filter ( Test_Frenetic_NetKAT_Parser.and3 (Test(Vlan(1))) (Test(VlanPcp(1))) (Test(TCPSrcPort(1))) ) 

TEST "policy_to_json serializes a NetKAT policy like example1.kat" = 
  let nk_policy = Frenetic_NetKAT_Parser.policy_from_string (In_channel.read_all "examples/example1.kat") in
  let example1_json = from_string (In_channel.read_all "lib_test/data/example1.json") in
  (to_string example1_json) = (to_string (policy_to_json nk_policy))

TEST "event_to_json serializes an OpenFlow Packet In event with a format like pkt_in.json" = 
  let pktin_event = Frenetic_NetKAT.PacketIn(
    "learning_switch", 
    0x871234L, 
    99l, 
    Frenetic_OpenFlow.Buffered(0l, Cstruct.of_string "Hi mom!"), 
    1024 
  ) in
  let pkt_in_json = from_string (In_channel.read_all "lib_test/data/pkt_in.json") in
  event_to_json pktin_event = pkt_in_json 

TEST "event_to_json serializes an OpenFlow Query event with a format like query.json" = 
  let query_event = Frenetic_NetKAT.Query(
    "ignored", 
    9823745L, 
    982734578L
  ) in
  let query_json = from_string (In_channel.read_all "lib_test/data/query.json") in
  event_to_json query_event = query_json 

TEST "event_to_json serializes an OpenFlow Switch Up event with a format like switch_up.json" = 
  let switch_up_event = Frenetic_NetKAT.SwitchUp(
    0x9878abcL, 
    [1l; 2l; 38l]
  ) in
  let switch_up_json = from_string (In_channel.read_all "lib_test/data/switch_up.json") in
  event_to_json switch_up_event = switch_up_json 

TEST "event_to_json serializes an OpenFlow Switch Down event with a format like switch_down.json" = 
  let switch_down_event = Frenetic_NetKAT.SwitchDown(
    0x9878abcL
  ) in
  let switch_down_json = from_string (In_channel.read_all "lib_test/data/switch_down.json") in
  event_to_json switch_down_event = switch_down_json 

TEST "event_to_json serializes an OpenFlow Port Up event with a format like port_up.json" = 
  let port_up_event = Frenetic_NetKAT.PortUp(
    0x9878abcL,
    1l
  ) in
  let port_up_json = from_string (In_channel.read_all "lib_test/data/port_up.json") in
  event_to_json port_up_event = port_up_json 

TEST "event_to_json serializes an OpenFlow Port Down event with a format like port_down.json" = 
  let port_down_event = Frenetic_NetKAT.PortDown(
    0x9878abdL,
    6l
  ) in
  let port_down_json = from_string (In_channel.read_all "lib_test/data/port_down.json") in
  event_to_json port_down_event = port_down_json 

(* Note: Link Up/Down and Host Up/Down are not OpenFlow events, but are synthesized through other events  *)
TEST "event_to_json serializes a Link Up event with a format like link_up.json" = 
  let link_up_event = Frenetic_NetKAT.LinkUp(
    (0x9878abdL, 6l),
    (0xabd9878L, 1l)
  ) in
  let link_up_json = from_string (In_channel.read_all "lib_test/data/link_up.json") in
  event_to_json link_up_event = link_up_json 

TEST "event_to_json serializes a Link Down event with a format like link_down.json" = 
  let link_down_event = Frenetic_NetKAT.LinkDown(
    (0xabd9878L, 1l),
    (0x9878abdL, 6l)
  ) in
  let link_down_json = from_string (In_channel.read_all "lib_test/data/link_down.json") in
  event_to_json link_down_event = link_down_json 

TEST "event_to_json serializes a Host Up event with a format like host_up.json" = 
  let host_up_event = Frenetic_NetKAT.HostUp(
    (0xabd9878L, 1l),
    (0x786ab8979873L, 0x12345678l)
  ) in
  let host_up_json = from_string (In_channel.read_all "lib_test/data/host_up.json") in
  event_to_json host_up_event = host_up_json 

TEST "event_to_json serializes a Host Down event with a format like host_up.json" = 
  let host_down_event = Frenetic_NetKAT.HostDown(
    (0xabd9878L, 1l),
    (0x786ab8979873L, 0x12345678l)
  ) in
  let host_down_json = from_string (In_channel.read_all "lib_test/data/host_down.json") in
  event_to_json host_down_event = host_down_json 

TEST "stats_to_json serializes an OpenFlow stats response like stats.json" = 
  let stats_response = (0xaceL, 0xace45L) in
  let stats_json = from_string (In_channel.read_all "lib_test/data/stats.json") in
  stats_to_json stats_response = stats_json 

let sample_port_stats_response = 
  let open Frenetic_OpenFlow0x01 in 
  [ { 
    port_no = 1000
    ; rx_packets = 2000000000L
    ; tx_packets = 3000000000L
    ; rx_bytes = 4000000000L
    ; tx_bytes = 5000000000L
    ; rx_dropped = 6000000000L
    ; tx_dropped = 7000000000L
    ; rx_errors = 8000000000L
    ; tx_errors = 9000000000L
    ; rx_frame_err = 10000000000L
    ; rx_over_err = 11000000000L
    ; rx_crc_err = 12000000000L
    ; collisions = 13000000000L
  }]

TEST "port_stats_to_json serializes an OpenFlow stats response like port_stats.json" = 
  let port_stats_json = from_string (In_channel.read_all "lib_test/data/port_stats.json") in
  port_stats_to_json sample_port_stats_response = port_stats_json 

TEST "policy_from_json_string deserializes a NetKAT-Json string from example1 into a NetKAT policy" =
  let nk_policy = Frenetic_NetKAT_Parser.policy_from_string (In_channel.read_all "examples/example1.kat") in
  let example1_json_str = In_channel.read_all "lib_test/data/example1_reversed.json" in
  policy_from_json_string example1_json_str = nk_policy

TEST "event_to_json_string serializes an OpenFlow Query event with a format like query.json" = 
  let query_event = Frenetic_NetKAT.Query(
    "ignored", 
    9823745L, 
    982734578L
  ) in
  let query_json_str = In_channel.read_all "lib_test/data/query.json" in
  event_to_json_string query_event = query_json_str

TEST "policy_to_json_string serializes a NetKAT policy like example1.kat" = 
  let nk_policy = Frenetic_NetKAT_Parser.policy_from_string (In_channel.read_all "examples/example1.kat") in
  let example1_json_str = In_channel.read_all "lib_test/data/example1.json" in
  example1_json_str = policy_to_json_string nk_policy

TEST "stats_to_json_string serializes an OpenFlow stats response like stats.json" = 
  let stats_response = (0xaceL, 0xace45L) in
  let stats_json_str = In_channel.read_all "lib_test/data/stats.json" in
  stats_to_json_string stats_response = stats_json_str 

TEST "port_stats_to_json_string serializes an OpenFlow stats response like port_stats.json" = 
  let port_stats_json_str = In_channel.read_all "lib_test/data/port_stats.json" in
  port_stats_to_json_string sample_port_stats_response = port_stats_json_str

