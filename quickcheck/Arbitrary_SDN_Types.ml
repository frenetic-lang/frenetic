open SDN_Types

open QuickCheck
module Gen = QuickCheck_gen

let arbitrary_bytes max_len =
  let open Gen in
  choose_int32 (Int32.zero, Int32.of_int max_len) >>= fun l ->
    ret_gen (Cstruct.create (Int32.to_int l))

let arbitrary_bufferId = Arbitrary_Base.arbitrary_uint32

let arbitrary_payload =
  let open Gen in
  arbitrary_bytes 1024 >>= fun bytes ->
  oneof [
    arbitrary_bufferId >>= (fun id -> ret_gen (SDN_Types.Buffered (id, bytes)));
    ret_gen (SDN_Types.NotBuffered bytes)]
