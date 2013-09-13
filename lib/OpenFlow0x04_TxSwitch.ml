module Sw = OpenFlow0x04_Switch
module OF = OpenFlow0x04
module OFC = OpenFlow0x04_Core
module Message = OF.Message
module Log = Lwt_log

type file_descr = Lwt_unix.file_descr
type xid = OFC.xid
type msg = Message.t



type t = {
	sw : OpenFlow0x04_Switch.t;
	handlers : (xid, msg Lwt.u) Hashtbl.t;
	mutable available_xid : xid;
	recv_others : msg Lwt_stream.t
}

let from_switch (sw : OpenFlow0x04_Switch.t) : t =
	let handlers = Hashtbl.create 100 in
	let (recv_others, push_other) = Lwt_stream.create () in
	let rec recv_thread () =
	  lwt (xid, msg) = Sw.recv sw in
	  (if Hashtbl.mem handlers xid then
	      Lwt.wakeup (Hashtbl.find handlers xid) msg
	   else
	     push_other (Some msg));
	  recv_thread () in
	Lwt.async (fun () ->
    Lwt.pick [ recv_thread (); Sw.wait_disconnect sw ]);
	{ sw = sw; handlers = handlers; available_xid = 1l;
	  recv_others = recv_others }

let send (sw : t) (msg : msg) : unit Lwt.t =
	Sw.send sw.sw 0l msg

let recv_stream (sw : t) =
	sw.recv_others

let transaction (sw : t) (msg : msg) : msg Lwt.t =
	let xid = sw.available_xid in
	(* increment, allow overflow, then skip zero *)
	sw.available_xid <- if Int32.add xid 1l = 0l then 1l else Int32.add xid 1l;
  let (recv_thread, recv_waker) = Lwt.wait () in
  Hashtbl.add sw.handlers xid recv_waker;
  lwt () = Sw.send sw.sw xid msg in
  recv_thread
