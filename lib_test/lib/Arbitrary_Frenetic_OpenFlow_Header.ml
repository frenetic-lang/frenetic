open QuickCheck
open Arbitrary_Base
module Gen = QuickCheck_gen

module Header = struct
  type t = Frenetic_OpenFlow_Header.t

  let arbitrary =
    let open Gen in
    let open Frenetic_OpenFlow_Header in
    arbitrary_uint8 >>= fun version ->
    arbitrary_uint8 >>= fun type_code ->
    arbitrary_uint16 >>= fun length ->
    arbitrary_uint32 >>= fun xid ->
      ret_gen { version; type_code; length; xid }

  let to_string = Frenetic_OpenFlow_Header.to_string

  let parse = Frenetic_OpenFlow_Header.parse
  let marshal x y = Frenetic_OpenFlow_Header.marshal y x; Frenetic_OpenFlow_Header.size
  let size_of _ = Frenetic_OpenFlow_Header.size
end
