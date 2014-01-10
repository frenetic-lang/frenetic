open QuickCheck
open OpenFlow0x01_Arbitrary
module Gen = QuickCheck_gen

module Header = struct
  type t = OpenFlow_Header.t

  let arbitrary =
    let open Gen in
    let open OpenFlow_Header in
    arbitrary_uint8 >>= fun version ->
    arbitrary_uint8 >>= fun type_code ->
    arbitrary_uint16 >>= fun length ->
    arbitrary_uint32 >>= fun xid ->
      ret_gen { version; type_code; length; xid }

  let to_string = OpenFlow_Header.to_string

  let parse = OpenFlow_Header.parse
  let marshal x y = OpenFlow_Header.marshal y x; OpenFlow_Header.size
  let size_of _ = OpenFlow_Header.size
end
