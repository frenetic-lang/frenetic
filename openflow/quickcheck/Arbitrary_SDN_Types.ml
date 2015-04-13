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

let arbitrary_ip_mask = 
  let open Gen in 
  let open Arbitrary_Packet in 
  arbitrary_nwAddr >>= fun p ->
  choose_int32 (0l, 32l) >>= fun m -> 
    ret_gen (p,m)

let arbitrary_pattern =
  let open Gen in
  let open Pattern in
  let open Arbitrary_Packet in
  let opt = Arbitrary_Base.arbitrary_option in
  let arbitrary_inPort = Arbitrary_Base.arbitrary_uint32 in
  opt arbitrary_dlAddr >>= fun dlSrc ->
  opt arbitrary_dlAddr >>= fun dlDst ->
  opt arbitrary_dlTyp  >>= fun dlTyp ->
  arbitrary_dlVlan     >>= fun (dlVlan, _, dlVlanPcp) ->
  opt arbitrary_ip_mask >>= fun nwSrc ->
  opt arbitrary_ip_mask >>= fun nwDst ->
  opt arbitrary_nwProto >>= fun nwProto ->
  opt arbitrary_tpPort >>= fun tpSrc ->
  opt arbitrary_tpPort >>= fun tpDst ->
  opt arbitrary_inPort >>= fun inPort ->
  ret_gen { dlSrc = dlSrc
          ; dlDst = dlDst
          ; dlTyp = dlTyp
          ; dlVlan = dlVlan
          ; dlVlanPcp = (match dlVlan with
            | Some _ -> Some(dlVlanPcp)
            | None   -> None)
          ; nwSrc = nwSrc
          ; nwDst = nwDst
          ; nwProto = nwProto
          ; tpSrc = tpSrc
          ; tpDst = tpDst
          ; inPort = inPort }
