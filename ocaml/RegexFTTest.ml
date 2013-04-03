open OpenFlow0x04Types
open WordInterface
open Platform0x04
open FaultTolerance
open NetCoreFT
open Regex

module G = Graph.Graph

module D = DiamondTopo

module Routing = struct

    (* Diamond topology *)
    (* S1
       /\
     S2 S3
      \ /
       S4
    *)

  let (policy, push) = Lwt_stream.create ()

  let ints_to_ipv4 (a,b,c,d) =
    let (|||) x y = Int32.logor x y in
    let (<<<) x y = Int32.shift_left x y in
    let a = Int32.of_int a in
    let b = Int32.of_int b in
    let c = Int32.of_int c in
    let d = Int32.of_int d in
    (a <<< 24) ||| (b <<< 16) ||| (c <<< 8) ||| d

  let make_host_ip i = ints_to_ipv4 (10,0,0,i)
  let h1 = 1
  let h2 = 2

  let s1 = Int64.of_int 1
  let s2 = Int64.of_int 2
  let s3 = Int64.of_int 3
  let s4 = Int64.of_int 4

  let from_to i j = And (SrcIP (make_host_ip i), DstIP (make_host_ip j))
			   (* (DlType 0x800)) *)
  let make_policy = RegUnion (RegPol (from_to 1 2, (Sequence (Host h1, Sequence (Star, Host h2))), 1),
			    RegPol (from_to 2 1, (Sequence (Host h2, Sequence (Star, Host h1))), 1))

  let desugar_group_htbl tbl =
    Hashtbl.fold (fun sw swTbl acc -> 
      let () = Hashtbl.add acc sw (List.map (fun (a,b, acts) -> (a,b, List.map (List.map NetCoreFT.desugar_act) acts)) swTbl) in
    acc) tbl (Hashtbl.create 10)
  (** Composes learning and routing policies, which together form
      mac-learning. *)      
  let groups_to_string groups =
    String.concat ";\n" (List.map (fun (gid,_,acts) -> Printf.sprintf "%ld" gid) groups)

  let group_htbl_to_str ghtbl =
    String.concat "" (H.fold (fun sw groups acc -> (Printf.sprintf "%Ld -> [\n%s]\n" sw (groups_to_string groups)):: acc) ghtbl [])

  let () = let pol,groups = compile_ft_to_nc make_policy (D.make_topo ()) in
	   let dgroups = desugar_group_htbl groups in
	   Printf.printf "%s\n" (policy_to_string pol);
	   Printf.printf "groups: %s\n" (group_htbl_to_str groups);
	   Printf.printf "dgroups: %s\n" (group_htbl_to_str dgroups);
	   push (Some (pol, dgroups))
end

module Make (Platform : PLATFORM) = struct

  module Controller = NetCoreFT.Make (Platform)

  let start () = Controller.start_controller Routing.policy

end

