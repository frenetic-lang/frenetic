open Core.Std

module Platform = Async_OpenFlow_Platform
module Header = OpenFlow_Header
module M = OpenFlow0x01.Message

module Message : Platform.Message with type t = (OpenFlow0x01.xid * M.t) = struct

  type t = (OpenFlow0x01.xid * M.t) sexp_opaque with sexp

  let header_of (xid, m) = M.header_of xid m
  let parse hdr buf = M.parse hdr (Cstruct.to_string buf)
  let marshal (xid, m) buf = M.marshal_body m buf
  let to_string (_, m) = M.to_string m

end
