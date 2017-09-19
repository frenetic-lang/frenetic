open Core
open Frenetic_OpenFlow_Header

let%test "size returns 8 bytes" =
  size = 8

let%test "parse rejects buffer less than size" =
  Exn.does_raise (fun () ->
    let buf = Cstruct.of_string "\x04" in
    let _ = parse buf in
    false
  )
  
let%test "parse correctly parses an OpenFlow header" =
  let buf = Cstruct.of_string "\x04\x03\x00\x40\x12\x34\x56\x78\x99\x99" in
  let parsed = parse buf in
  parsed = { version = 4; type_code = 3; length = 64; xid = 0x12345678l }

let%test "marshal correctly converts an OpenFlow header" =
  let buf = Cstruct.create size in
  let () = marshal buf { version = 4; type_code = 3; length = 64; xid = 0x12345678l } in
  buf = Cstruct.of_string "\x04\x03\x00\x40\x12\x34\x56\x78"

let%test "to_string returns human readable rep of header" =
  let hdr = { version = 4; type_code = 3; length = 64; xid = 0x12345678l } in
  to_string hdr = "{ version = 4, code = 3, len = 64, xid = 305419896 }"

