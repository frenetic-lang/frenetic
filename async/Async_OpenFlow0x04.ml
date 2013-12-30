open Core.Std

module Platform = Async_OpenFlow_Platform
module Header = OpenFlow_Header
module M = OpenFlow0x04.Message

module Message : Platform.Message with type t = (Header.xid * M.t) = struct

  type t = (Header.xid * M.t) sexp_opaque with sexp

  let header_of (xid, m)= M.header_of xid m
  let parse hdr buf = M.parse hdr (Cstruct.to_string buf)
  let marshal (xid, m) buf = M.marshal_body m buf
  let to_string _ = "we do not have an implementation of to_string for 1.3"

  let marshal' msg =
    let hdr = header_of msg in
    let body_len = hdr.Header.length - Header.size in
    let body_buf = Cstruct.create body_len in
    marshal msg body_buf;
    (hdr, body_buf)
end

include Async_OpenFlow_Message.MakeSerializers (Message)
