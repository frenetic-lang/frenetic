open Async.Std
open Core.Std

module Log = Async_OpenFlow_Log
module Header = OpenFlow_Header

module type Message = sig
  type t
  include Sexpable with type t := t

  val header_of : t -> OpenFlow_Header.t

  val parse : OpenFlow_Header.t -> Cstruct.t -> t

  val marshal : t -> Cstruct.t -> int

  val to_string : t -> string
end

module type S = sig

  type t 
  type m

  module Switch_id: Unique_id

  type result = [
    | `Connect of Switch_id.t
    | `Disconnect of Switch_id.t * Sexp.t
    | `Message of Switch_id.t * m
  ]

  val create
    :  ?max_pending_connections:int
    -> ?verbose:bool
    -> ?log_disconnects:bool
    -> ?buffer_age_limit:[ `At_most of Time.Span.t | `Unlimited ]
    -> port:int
    -> t Deferred.t

  val listen : t -> result Pipe.Reader.t

  val close : t -> Switch_id.t -> unit

  val has_switch_id : t -> Switch_id.t -> bool

  val send 
    : t
    -> Switch_id.t
    -> m
    -> [ `Drop of exn | `Sent of Time.t ] Deferred.t

  val send_to_all : t -> m -> unit

  val client_addr_port 
    :  t 
    -> Switch_id.t
    -> (Unix.Inet_addr.t * int) option

  val listening_port : t -> int

end

module Make(Message : Message) = struct

  type m = Message.t

  module Impl = Typed_tcp.Make(struct

    module Client_message = Message
    module Server_message = Message

    let deserialize (raw_reader : Reader.t) : [ `Eof | `Ok of m] Deferred.t = 
      let ofhdr_str = String.create Header.size in
      Reader.really_read raw_reader ofhdr_str
      >>= function
      | `Eof _ -> 
        Log.info ~tags:[("openflow", "serialization")]
          "EOF reading OpenFlow header";
        return `Eof
      | `Ok ->
        let hdr = Header.parse (Cstruct.of_string ofhdr_str) in
        let body_len = hdr.Header.length - Header.size in
        let body_buf = String.create body_len in
        Reader.really_read raw_reader body_buf
        >>= function
        | `Eof _ ->
          Log.info ~tags:[("openflow", "serialization")]         
            "EOF reading message body (expected %d bytes)"
            body_len;
          return `Eof
        | `Ok ->
          let m = Message.parse hdr (Cstruct.of_string body_buf) in
          Log.of_lazy ~level:`Debug ~tags:[("openflow", "serialization")] 
            (lazy (sprintf "read message: %s" (Message.to_string m)));
          return (`Ok m)

    module Transport = struct

      type t = Reader.t * Writer.t

      let create (r : Reader.t) (w : Writer.t) = return (r, w)

      let close ((_, w) : t) = Writer.close w

      let flushed_time ((_, w) : t) = Writer.flushed_time w

      let read ((r, _) : t) = deserialize r

      let write ((_, w) : t) (m : m) : unit =
        let buf = Cstruct.create (Message.header_of m).Header.length in
        let _ = Message.marshal m buf in
        Async_cstruct.schedule_write w buf     

    end
  
  end)

  type t = Impl.t

  module Switch_id =  Impl.Client_id

  type result = [
    | `Connect of Switch_id.t
    | `Disconnect of Switch_id.t * Sexp.t
    | `Message of Switch_id.t * m
  ]

  let create ?max_pending_connections ?verbose ?log_disconnects
    ?buffer_age_limit ~port =
    Impl.create ?max_pending_connections ?verbose ?log_disconnects
      ?buffer_age_limit ~port ~auth:(fun _ _ -> return `Allow) ()

  let listen t =
    let open Impl.Server_read_result in
    Pipe.map (Impl.listen t)
    ~f:(function
        | Connect id -> `Connect id
        | Disconnect (id, sexp) -> `Disconnect (id, sexp)
        | Denied_access msg -> raise (Invalid_argument "Denied_access should not happen")
        | Data (id, m) -> `Message (id, m))

  let close = Impl.close

  let has_switch_id = Impl.has_client_id

  let send = Impl.send

  let send_to_all = Impl.send_to_all

  let client_addr_port = Impl.client_addr_port

  let listening_port = Impl.port

end