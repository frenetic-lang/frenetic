open OpenFlow0x01

open QuickCheck
module Gen = QuickCheck_gen

(* arbitrary instance for usigned integers. Still uses the `int` type. *)
let arbitrary_uint = Gen.sized (fun n -> Gen.choose_int (0, n))

module type OpenFlow0x01_Arbitrary = sig
    type t
    type s

    val arbitrary : t arbitrary

    val to_string : t -> string

    val parse : s -> t
    val marshal : t -> s
end

module Wildcards = struct
  type t = Wildcards.t
  type s = Int32.t

  let arbitrary : t arbitrary =
    let open Gen in
    let open Wildcards in
    arbitrary_bool >>= fun in_port ->
    arbitrary_bool >>= fun dl_vlan ->
    arbitrary_bool >>= fun dl_src ->
    arbitrary_bool >>= fun dl_dst ->
    arbitrary_bool >>= fun dl_type ->
    arbitrary_bool >>= fun nw_proto ->
    arbitrary_bool >>= fun tp_src ->
    arbitrary_bool >>= fun tp_dst ->
    arbitrary_uint >>= fun nw_src ->
    arbitrary_uint >>= fun nw_dst ->
    arbitrary_bool >>= fun dl_vlan_pcp ->
    arbitrary_bool >>= fun nw_tos ->
        ret_gen {
            in_port = in_port;
            dl_vlan = dl_vlan;
            dl_src = dl_src;
            dl_dst = dl_dst;
            dl_type = dl_type;
            nw_proto = nw_proto;
            tp_src = tp_src;
            tp_dst = tp_dst;
            nw_src = nw_src;
            nw_dst = nw_dst;
            dl_vlan_pcp = dl_vlan_pcp;
            nw_tos = nw_tos
          }

    let to_string = Wildcards.to_string

    let parse = Wildcards.parse
    let marshal = Wildcards.marshal
end
