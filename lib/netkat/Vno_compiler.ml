open Core
open Syntax

type fabric = FabricGen.FabricGen.fabric
module Virtual_Compiler = Virtual_Compiler.Make(FabricGen.FabricGen)

type vno =
  { vtopo: policy;
    ving: pred;
    veg: pred;
    vrel: pred;
    ving_pol: policy }
(** A virtual network object, not including a policy *)

type t = {
  topo : policy;
  ing : pred;
  eg : pred;
  vnos : (vno * fabric) Int.Map.t;
  vpols : policy Int.Map.t;
}
(** A physical network together with some VNOs *)

let create ~topo ~ing ~eg =
  let vnos = Int.Map.empty in
  let vpols = Int.Map.empty in
  { topo; ing; eg; vnos; vpols }

let add_vno ~id ~vtopo ~ving ~veg ~vrel ?ving_pol t : t =
  let ving_pol = match ving_pol with
    | Some p -> p
    | None ->
      begin match FabricGen.FabricGen.default_ving_pol ~vrel ~ping:t.ing with
      | Some p -> p
      | None -> failwith "unable to infer ving_pol automatically"
      end
  in
  let vno = { vtopo; ving; veg; vrel; ving_pol } in
  let fabric = Virtual_Compiler.generate_fabric
    ~vrel ~vtopo ~ving ~veg
    ~ptopo:t.topo ~ping:t.ing ~peg:t.eg
    ?log:None ?record_paths:None
  in
  let vnos = Map.add t.vnos ~key:id ~data:(vno, fabric) in
  { t with vnos = vnos }

let set_pol ~id ~pol t : t =
  let vpols = Map.add t.vpols ~key:id ~data:pol in
  { t with vpols }

let compile_vno t (id, ({ vtopo; ving; veg; vrel; ving_pol }, fabric)) : policy =
  let vpol = Map.find t.vpols id |> Option.value ~default:drop in
  Virtual_Compiler.compile_with_fabric fabric
    ~vtopo ~ving_pol ~ving ~veg ~ping:t.ing ~peg:t.eg ~vpol

let compile ?options ?pc t : Local_compiler.FDD.t =
  Map.to_alist t.vnos
  |> List.map ~f:(compile_vno t)
  |> Optimize.mk_big_union
  |> Global_compiler.compile ?options ?pc ~ing:t.ing
