open MessagesDef
open OpenFlow0x04Parser
open OpenFlow0x04Types
open PatternImplDef
open NetCoreEval13
open NetCoreCompiler13
open Classifier
open List

module W = Wildcard

(* Egh. Assuming each action list only has a single port. Otherwise we
   need to use watch groups instead of a watch port and that's just more
   messy crap *)

let ofpp_any = Int32.of_int (-1)
(* Not even specified in OF 1.3. It's like they're not even trying... *)
let ofpg_any = Int32.of_int (-1)

(* let rec watchport acts = match acts with *)
(*   | NetCoreEval.Forward (modif, MessagesDef.PhysicalPort pid) :: acts -> (Int32.of_int pid) *)
(*   | _ :: acts -> watchport acts *)
(*   | _ -> ofpp_any *)

let rec mapi' idx f lst = match lst with
  | elm :: lst -> f idx elm :: mapi' (idx+1) f lst
  | [] -> []

let mapi f lst = mapi' 0 f lst

(* let insert_groups = mapi (fun idx (pat,(a,b)) -> (pat, (Int32.of_int idx, [(0, (watchport a), a); (0, (watchport b), b)]))) *)

let rm_inport = let project ((pat, acts) : Pattern.pattern * actions list) = ({ pat with ptrnInPort = W.WildcardAll }, acts) in
		      map project

let compile_nc = compile_opt

module Gensym =
struct
  let count = ref (Int32.of_int 0)
  let next () = count := Int32.succ !count; !count
end

let add_buckets group a b =
  group := [a;b] :: !group

let rec compile_pb pri bak sw =
  let pri_tbl = compile_nc pri sw in
  let bak_tbl = compile_nc bak sw in
  let groups = ref [] in
  let merge a b = (let () = add_buckets groups a b in
		   [Group (Gensym.next ())]) in
  let ft_tbl = inter merge pri_tbl (rm_inport bak_tbl) in
  (ft_tbl @ pri_tbl @ bak_tbl, !groups)
    
