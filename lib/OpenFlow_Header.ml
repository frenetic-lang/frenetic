open Core.Std

type xid = Int32.t

type t = {
  version: int;
  type_code: int;
  length: int;
  xid: Int32.t (* if xid, then with sexp complains. The .mli can use xid
                  here without any problem. *)
} with sexp

cstruct ofp_header {
  uint8_t version;
  uint8_t typ;
  uint16_t length;
  uint32_t xid
} as big_endian

let size = sizeof_ofp_header

let parse (buf : Cstruct.t) : t =
  assert (Cstruct.len buf >= size);
  {
    version = get_ofp_header_version buf;
    type_code = get_ofp_header_typ buf;
    length = get_ofp_header_length buf;
    xid = get_ofp_header_xid buf
  }

let marshal (buf : Cstruct.t) (t : t) : unit =
  assert (Cstruct.len buf >= size);  
  set_ofp_header_version buf t.version;
  set_ofp_header_typ buf t.type_code;
  set_ofp_header_length buf t.length;
  set_ofp_header_xid buf t.xid

let to_string hdr =
  Format.sprintf "{ version = %x, code = %d, len = %d, xid = %ld }"
    hdr.version hdr.type_code hdr.length hdr.xid
