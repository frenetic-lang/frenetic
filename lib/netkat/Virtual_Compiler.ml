open Syntax
open FabricGen
open Optimize
open Pretty

module Tbl = Core.Hashtbl.Poly
module Sexp = Core.Sexp

module Make(FG:FABRIC_GEN) =
struct
  include FG

(*
  Vingress defines the virtual ingress. Examples:

   1) The physical ingress is {(1,1)} (i.e. packets can enter the network only
      through port 1 of switch 1), and we want packets to enter the virtual network
      at vport 3 of vswitch 3. This is encoded as
        vingress =
          vswitch := 3; vport := 3

   2) The physical ingress is {(1,1), (2,2)} (i.e. packets can enter the network only
      through port 1 of switch 1 and port 2 of switch 2),
      and we want packets to enter the virtual network
      at vport 3 of vswitch 3. This is encoded as
        vingress =
          vswitch := 3; vport := 3

   3) The physical ingress is {(1,1)} and we want packets to enter the virtual network
      at both vport 3 of vswitch 3 and vport 4 of switch 4. This is encoded as
        vingress =
          (vswitch := 3; vport := 3) + (vswitch := 4; vport := 4)

   4) The physical ingress is {(1,1), (2,2)} and we want packets from (1,1) to
      enter the virtual network at vport 3 of vswitch 3, and packet from (2,2)
      shall enter at vport 4 of vswitch 4. This is encoded as
        vingress =
          switch = 1; port = 1; vswitch := 3; vport := 3
        + switch = 2; port = 2; vswitch := 4; vport := 4

   5) I just realized that the framework can even handle more complicated virtual ingress
      specifications as the ones Arjun mentioned in our last meeting, e.g.
        vingress =
          IPProto = tcp; vswitch := 1; vport := 1
        + IPProto = ucp; vswitch := 2; vport := 1
      This is super awesome!!


  To gurantee correctness we will have to do some sort of "type checking", i.e. we have to make sure
  certain pre conditions are met.

*)

  let rec encode_vlinks (vtopo : policy) =
    match vtopo with
    | Union (p, q) -> mk_union (encode_vlinks p) (encode_vlinks q)
    | Seq (p, q) -> mk_seq (encode_vlinks p) (encode_vlinks q)
    | Star p -> mk_star (encode_vlinks p)
    | VLink (vsw1, vpt1, vsw2, vpt2) ->
      mk_seq
        (mk_filter (mk_and (Test (VSwitch vsw1)) (Test (VPort vpt1))))
        (mk_seq (Mod (VSwitch vsw2)) (Mod (VPort vpt2)))
    | _ -> vtopo

  let compile_with_fabric
      ~(vtopo : policy) ~(ving_pol : policy) ~(ving : pred) ~(veg : pred)
      ~(ping : pred) ~(peg : pred)
      ~(vpol : policy) (fabric : FG.fabric) : policy =
    let (fout_set, fin_set) = fabric in
    let fout = mk_big_union fout_set in
    let fin = mk_big_union fin_set in
    let ing = mk_big_seq [Filter ping; ving_pol; Filter ving] in
    let eg = Filter (mk_and veg peg) in
    let p = mk_seq vpol fout in
    let t = mk_seq (encode_vlinks vtopo) fin in
    mk_big_seq [ing; mk_star (mk_seq p t); p; eg]

  let compile ?(log=true) ?(record_paths : string option)
      ~(vrel : pred) ~(vtopo : policy) ~(ving_pol : policy) ~(ving : pred) ~(veg : pred)
      ~(ptopo : policy)                                     ~(ping : pred) ~(peg : pred)
      (vpol : policy) : policy =
    generate_fabric ~log ?record_paths ~vrel ~vtopo ~ving ~veg ~ptopo ~ping ~peg
    |> compile_with_fabric ~vtopo ~ving_pol ~ving ~veg ~ping ~peg ~vpol
end
