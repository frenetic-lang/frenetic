open Core
open Frenetic_OpenFlow0x04
open Frenetic_GroupTable0x04

let%test "create creates a fresh group table" = 
  let gtab = create () in
  (to_string gtab) = ""

let rr_bucket_list = [ 
  { bu_weight = 1; bu_watch_port = None; bu_watch_group = None; bu_actions = [ Output(PhysicalPort(1l)) ] };
  { bu_weight = 1; bu_watch_port = None; bu_watch_group = None; bu_actions = [ Output(PhysicalPort(2l)) ] }
]

let%test "add_group adds a group to a group table" = 
  let gtab = create () in
  (* A round robin load balancer *)
  let _ = add_group gtab Select rr_bucket_list in
  (to_string gtab) =
    "ID=1, Type=Select, Buckets=[\n" ^ 
    "  weight 1: actions=(Output(PhysicalPort 1))\n" ^ 
    "  weight 1: actions=(Output(PhysicalPort 2))\n" ^
    "]"

let%test "clear_group removes all groups from a group table" = 
  let gtab = create () in
  let _ = add_group gtab Select rr_bucket_list in
  let () = clear_groups gtab in
  (to_string gtab) = ""

let%test "clear_group removes all groups from a group table" = 
  let gtab = create () in
  let _ = add_group gtab Select rr_bucket_list in
  let () = clear_groups gtab in
  (to_string gtab) = ""

let%test "commit emits OpenFlow 1.3 messages to recreate the group table on the switch" = 
  let gtab = create () in
  let _ = add_group gtab Select rr_bucket_list in
  let () = clear_groups gtab in
  let msgs = commit gtab in
  let msgs_as_str = String.concat (List.map ~f:Message.to_string msgs) in
  msgs_as_str = 
    "GroupMod = AddGroup { typ = Select; gid = 1 ; bucket = [ " ^
       "{ length = 32; weight = 1; watch_port = None; watch_group = None; actions = [ PseudoPort: PhysicalPort = 1 ] }; " ^
       "{ length = 32; weight = 1; watch_port = None; watch_group = None; actions = [ PseudoPort: PhysicalPort = 2 ] } " ^
       "] }" ^
    "GroupMod = DeleteGroup {type = Select; gid = 1 }"

let%test "add_fastfail_group forms fast fail group from a list of ports, most important first, creating appropriate action buckets" = 
  let gtab = create () in
  let _ = add_fastfail_group gtab [1l; 2l; 3l] in
  (to_string gtab) = 
    "ID=1, Type=FF, Buckets=[\n" ^
    "  weight 3: watch_port=1, actions=(Output(PhysicalPort 1))\n" ^
    "  weight 2: watch_port=2, actions=(Output(PhysicalPort 2))\n" ^
    "  weight 1: watch_port=3, actions=(Output(PhysicalPort 3))\n" ^
    "]"
