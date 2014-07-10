open Async.Std
open Core.Std

module Header = OpenFlow_Header
module type Message = Async_OpenFlow_Message.Message

exception Flush_closed_writer

module type S = sig

  type t 
  type m

  module Client_id : Unique_id

  type e = [
    | `Connect of Client_id.t
    | `Disconnect of Client_id.t * Sexp.t
    | `Message of Client_id.t * m
  ]

  val create
    :  ?max_pending_connections:int
    -> ?verbose:bool
    -> ?log_disconnects:bool
    -> ?buffer_age_limit:[ `At_most of Time.Span.t | `Unlimited ]
    -> port:int
    -> unit
    -> t Deferred.t

  val listen : t -> e Pipe.Reader.t

  val close : t -> Client_id.t -> unit

  val has_client_id : t -> Client_id.t -> bool

  val send 
    : t
    -> Client_id.t
    -> m
    -> [ `Drop of exn | `Sent of Time.t ] Deferred.t

  val send_ignore_errors : t -> Client_id.t -> m -> unit

  val send_to_all : t -> m -> unit

  val client_addr_port 
    :  t 
    -> Client_id.t
    -> (Unix.Inet_addr.t * int) option

  val listening_port : t -> int

end


module Make(Message : Message) = struct

  type m = Message.t

  module Impl = Typed_tcp.Make(struct

    module Client_message = Message
    module Server_message = Message

    module Serialization = Async_OpenFlow_Message.MakeSerializers (Message)

    module Transport = struct

      type t = Reader.t * Writer.t

      let create (r : Reader.t) (w : Writer.t) = return (r, w)

      let close ((_, w) : t) = Writer.close w

      let flushed_time ((_, w) : t) =
        let open Deferred in
        choose [ choice (Writer.flushed_time  w) (fun x  -> `F x)
               ; choice (Writer.consumer_left w) (fun () -> `C ())
               ]
        >>| function
          | `F x -> x
          | `C () -> raise Flush_closed_writer

      let read ((r, _) : t) = Serialization.deserialize r

      let write ((_, w) : t) (m : m) : unit = Serialization.serialize w m

    end
  
  end)

  type t = Impl.t

  module Client_id =  Impl.Client_id

  type e = [
    | `Connect of Client_id.t
    | `Disconnect of Client_id.t * Sexp.t
    | `Message of Client_id.t * m
  ]

  let create ?max_pending_connections
      ?verbose
      ?log_disconnects
      ?buffer_age_limit ~port () =
    Impl.create ?max_pending_connections ?verbose ?log_disconnects
      ?buffer_age_limit ~port ~auth:(fun _ _ _ -> return `Allow) ()

  let listen t =
    let open Impl.Server_read_result in
    Pipe.map (Impl.listen t)
    ~f:(function
        | Connect id            -> `Connect id
        | Disconnect (id, sexp) -> `Disconnect (id, sexp)
        | Data (id, m)          -> `Message (id, m)
        | Denied_access msg     -> assert false)

  let close = Impl.close

  let has_client_id = Impl.has_client_id

  let send t c_id m =
    Monitor.try_with (fun () -> Impl.send t c_id m)
    >>| function
      | Ok x       -> x
      | Error _exn -> `Drop _exn

  let send_ignore_errors = Impl.send_ignore_errors

  let send_to_all = Impl.send_to_all

  let client_addr_port = Impl.client_addr_port

  let listening_port = Impl.port

end
