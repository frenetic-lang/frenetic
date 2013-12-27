open Core.Std

module Platform = Async_OpenFlow_Platform
module Header = OpenFlow_Header
module M = OpenFlow0x04.Message

module Message : Platform.Message with type t = (Header.t * Cstruct.t) = struct

  type t = (Header.t * Cstruct.t) sexp_opaque with sexp

  let header_of (hdr, _) = hdr

  let parse hdr buf = (hdr, Cstruct.set_len buf (hdr.Header.length - Header.size))

  let marshal (hdr, body) buf = 
    Header.marshal buf hdr;
    Cstruct.blit body 0 buf Header.size (hdr.Header.length - Header.size);
    hdr.Header.length

  let to_string x = Sexp.to_string_hum (sexp_of_t x)

end