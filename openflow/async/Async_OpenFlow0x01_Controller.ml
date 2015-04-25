open Core.Std
open Async.Std
open OpenFlow0x01

type event = [
  | `Connect of switchId * SwitchFeatures.t
  | `Disconnect of switchId
  | `Message of switchId * OpenFlow_Header.t * Message.t
]

let (events, events_writer) = Pipe.create ()

let openflow_events (r:Reader.t) : (OpenFlow_Header.t * Message.t) Pipe.Reader.t =

  let reader,writer = Pipe.create () in

  let rec loop () =
    let header_str = String.create OpenFlow_Header.size in
    Reader.really_read r header_str >>= function 
      | `Eof _ -> 
        Pipe.close writer;
        return ()
      | `Ok -> 
        let header = OpenFlow_Header.parse (Cstruct.of_string header_str) in
        let body_len = header.length - OpenFlow_Header.size in
        let body_str = String.create body_len in
        Reader.really_read r body_str >>= function
          | `Eof _ -> 
            Pipe.close writer;
            return ()
          | `Ok -> 
            let _,message = Message.parse header body_str in
            Pipe.write_without_pushback writer (header,message);
            loop () in 
  don't_wait_for (loop ());
  reader

type switchState =
  { features : SwitchFeatures.t;
    send : xid -> Message.t -> unit;
    send_txn : Message.t -> (Message.t list) Deferred.t }

let switches : (switchId, switchState) Hashtbl.Poly.t =
  Hashtbl.Poly.create ()

type threadState =
  { switchId : switchId;
    txns : (xid, Message.t list Ivar.t * Message.t list) Hashtbl.t }

type state =
  | Initial
  | SentSwitchFeatures
  | Connected of threadState

let client_handler (a:Socket.Address.Inet.t) (r:Reader.t) (w:Writer.t) : unit Deferred.t =
  let open Message in 

  let serialize (xid:xid) (msg:Message.t) : unit = 
    let header = Message.header_of xid msg in 
    let buf = Cstruct.create header.length in 
    OpenFlow_Header.marshal buf header;
    Message.marshal_body msg (Cstruct.shift buf OpenFlow_Header.size);
    Async_cstruct.schedule_write w buf in 

  let my_events = openflow_events r in

  let rec loop (state:state) : unit Deferred.t =
    Pipe.read my_events >>= fun next_event ->
    match state, next_event with
    (* EchoRequest messages *)
    | _, `Ok (hdr,EchoRequest bytes) ->
      serialize 0l (EchoReply bytes);
      loop state
        
    (* Initial *)
    | Initial, `Eof -> 
      return () 
    | Initial, `Ok (hdr,Hello bytes) ->
      (* TODO(jnf): check version? *)
      serialize 0l SwitchFeaturesRequest;
      loop SentSwitchFeatures
    | Initial, `Ok(hdr,msg) -> 
      assert false

    (* SentSwitchFeatures *)
    | SentSwitchFeatures, `Eof -> 
      return ()
    | SentSwitchFeatures, `Ok (hdr, SwitchFeaturesReply features) ->
      let switchId = features.switch_id in
      let txns = Hashtbl.Poly.create () in 
      let xid_cell = ref 0l in 
      let next_uid () = xid_cell := Int32.(!xid_cell + 1l); !xid_cell in 
      let threadState = { switchId; txns } in
      let send xid msg = serialize xid msg in 
      let send_txn msg =
        let xid = next_uid () in 
        let ivar = Ivar.create () in 
        Hashtbl.Poly.add_exn txns ~key:xid ~data:(ivar,[]);
        Ivar.read ivar in
      Hashtbl.Poly.add_exn switches ~key:switchId ~data:{ features; send; send_txn };
      Pipe.write_without_pushback events_writer (`Connect (switchId, features));
      loop (Connected threadState)
    | SentSwitchFeatures, `Ok (hdr,msg) -> 
      assert false

    (* Connected *)
    | Connected threadState, `Eof ->
      (* TODO(jnf): log disconnection *)
      Hashtbl.Poly.remove switches threadState.switchId;
      Pipe.write_without_pushback events_writer (`Disconnect threadState.switchId);
      return ()
    | Connected threadState, `Ok (hdr, msg) ->
      Pipe.write_without_pushback events_writer (`Message (threadState.switchId, hdr, msg));
      (match Hashtbl.Poly.find threadState.txns hdr.xid with 
        | None -> ()
        | Some (ivar,msgs) -> 
          Hashtbl.Poly.remove threadState.txns hdr.xid;
          Ivar.fill ivar (msg::msgs));
      loop state in
  
  serialize 0l (Hello (Cstruct.create 0));
  loop Initial

let init port = 
  don't_wait_for 
    (Tcp.Server.create
     ~on_handler_error:`Raise
     (Tcp.on_port port)
     client_handler >>= fun _ -> 
     return ())

let get_switches () = 
  Hashtbl.Poly.keys switches

let get_switch_features switchId = 
  match Hashtbl.Poly.find switches switchId with 
    | Some switchState -> Some switchState.features
    | None -> None

let send switchId xid msg = 
  match Hashtbl.Poly.find switches switchId with
    | Some switchState -> 
      switchState.send xid msg; 
      `Ok
    | None ->
      `Eof

let send_txn switchId msg = 
  match Hashtbl.Poly.find switches switchId with
    | Some switchState -> 
      `Ok (switchState.send_txn msg)
    | None -> 
      `Eof
