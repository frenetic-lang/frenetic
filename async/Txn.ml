open Core.Std
open Async.Std

module Controller = Async_OpenFlow.OpenFlow0x01.Controller
module ClientTbl = Hashtbl.Make(Controller.Client_id)

module Xid = Int32
module XidTbl = Hashtbl.Make(Int32)

type r = [ `Disconnect of Sexp.t | `Result of OpenFlow0x01.Message.t ]

type t = {
  ctl : Controller.t;
  size : int;
  mutable xid : Xid.t;
  pending : r Ivar.t XidTbl.t ClientTbl.t
}

let next_xid t =
  let xid = Int32.(t.xid +1l) in
  t.xid <- xid;
  xid

(** [send t c_id msg] will send [msg] to the specified client and return an
 * Ivar.t that will become determined when the response is received. *)
let send t c_id (msg : OpenFlow0x01.Message.t) =
  let xid = next_xid t in
  let ivar = Ivar.create () in
  XidTbl.add_exn (ClientTbl.find_exn t.pending c_id) xid ivar;
  Controller.send t.ctl c_id (xid, msg)
  >>| function
    | `Sent _   -> `Sent ivar
    | `Drop exn ->
      begin match ClientTbl.find t.pending c_id with
      | Some(xids) ->
        XidTbl.remove xids xid
      | None -> ()
      end;
      `Drop exn

let send_switch t sw_id (msg : OpenFlow0x01.Message.t) =
  send t (Controller.client_id_of_switch_exn t.ctl sw_id) msg

(** [create ctl] returns a Transaction.t, which is the ctl object used to
 * interact with the instance.
 *
 * The public API for this type is [stage] and [send].
 * *)
let create ?(size=10) (ctl : Controller.t) : t =
  { ctl; xid = 0l; pending = ClientTbl.create ~size (); size = size }

(** [stage t evt] returns a Stage.t that should be sequenced with other stages
 * that comprise the controller being built. This stage listens for responses
 * and fills the appropriate Ivar.t.
 * *)
let stage t evt =
  begin match evt with
  | `Connect(c_id, feats) ->
    ClientTbl.add_exn t.pending c_id (XidTbl.create ~size:t.size ());
    return [evt]
  | `Disconnect(c_id, sw_id, exn_) ->
    begin match ClientTbl.find_and_remove t.pending c_id with
    | Some(xids) ->
      XidTbl.iter_vals xids ~f:(fun ivar -> Ivar.fill ivar (`Disconnect exn_))
    | None -> ()
    end;
    return [evt]
  | `Message(c_id, (xid, (msg : OpenFlow0x01.Message.t))) when not (xid = 0l) ->
    let xids = ClientTbl.find_exn t.pending c_id in
    begin match XidTbl.find_and_remove xids xid with
    | Some(ivar) ->
      Ivar.fill ivar (`Result msg);
      return []
    | None ->
      return [evt]
    end
  | `Message _ ->
    return [evt]
  end
