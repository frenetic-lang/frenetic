open OpenFlow0x04
open OpenFlow0x04_Core

type t = {
	table : (groupId, (groupType * bucket list)) Hashtbl.t;
	mutable next_group_id : groupId;
	mutable pending_messages : Message.t list
}

let create () : t = {
	table = Hashtbl.create 100;
	next_group_id = 0l;
  pending_messages = []
}

let add_group (tbl : t) (typ : groupType) (buckets : bucket list) : groupId =
  let id = tbl.next_group_id in
  tbl.next_group_id <- Int32.succ id;
  if tbl.next_group_id = 0l then
    failwith "out of group IDs"
  else
    let msg = Message.GroupModMsg (AddGroup (typ, id, buckets)) in
    Hashtbl.add tbl.table id (typ, buckets);
    tbl.pending_messages <- msg :: tbl.pending_messages;
    id

let clear_groups (tbl : t) : unit =
	tbl.next_group_id <- 0l;
	let rm_group (id : groupId) ((typ, _) : groupType * bucket list) : unit =
	  let msg = Message.GroupModMsg (DeleteGroup (typ, id)) in
	  tbl.pending_messages <-  msg :: tbl.pending_messages in
  Hashtbl.iter rm_group tbl.table;
  Hashtbl.clear tbl.table

let commit (tbl : t) : Message.t list =
	let msgs = tbl.pending_messages in
	tbl.pending_messages <- [];
	List.rev msgs