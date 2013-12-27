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

  (* Uses OCaml's built-in digest module *)
  let readable_md5 (buf : string) : string =
    Digest.to_hex (Digest.string buf)

  let deserialize ?(tags : (string * string) list = []) ?(label : string = "")
    (raw_reader : Reader.t) : [ `Eof | `Ok of M.t] Deferred.t = 
    let log msg = Log.of_lazy ~level:`Debug ~tags:tags msg in
    let ofhdr_str = String.create Header.size in
    Reader.really_read raw_reader ofhdr_str
    >>= function
    | `Eof _ -> 
      log (lazy (sprintf "[%s] EOF reading header" label));
      return `Eof
    | `Ok ->
      let hdr = Header.parse (Cstruct.of_string ofhdr_str) in
      let body_len = hdr.Header.length - Header.size in
      let body_buf = String.create body_len in
      Reader.really_read raw_reader body_buf
      >>= function
      | `Eof _ ->
        log (lazy (sprintf "[%s] EOF reading body (expected %d bytes)" 
                     label body_len));
        return `Eof
      | `Ok ->
        let m = M.parse hdr (Cstruct.of_string body_buf) in
        log (lazy (sprintf "[%s] read message header=%s; body hash=%s"
                     label
                     (Header.to_string hdr)
                     (readable_md5 (ofhdr_str ^ body_buf))));
        return (`Ok m)

  let serialize ?(tags : (string * string) list = []) ?(label : string = "")
    (raw_writer : Writer.t) (m : M.t) : unit =
    let buf = Cstruct.create (M.header_of m).Header.length in
    let _ = M.marshal m buf in
    Async_cstruct.schedule_write raw_writer buf;
    Log.of_lazy ~level:`Debug ~tags:tags
      (lazy (sprintf "[%s] wrote message hash=%s" 
               label (readable_md5 (Cstruct.to_string buf))))

end