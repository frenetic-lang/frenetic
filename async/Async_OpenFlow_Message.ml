open Core.Std
open Async.Std

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

module MakeSerializers (M : Message) = struct

  let deserialize (raw_reader : Reader.t) : [ `Eof | `Ok of M.t] Deferred.t = 
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
        let m = M.parse hdr (Cstruct.of_string body_buf) in
        Log.of_lazy ~level:`Debug ~tags:[("openflow", "serialization")] 
          (lazy (sprintf "read message: %s" (M.to_string m)));
        return (`Ok m)

  let serialize (raw_writer : Writer.t) (m : M.t) : unit =
    let buf = Cstruct.create (M.header_of m).Header.length in
    let _ = M.marshal m buf in
    Async_cstruct.schedule_write raw_writer buf

end