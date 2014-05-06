module Run = struct
  open Core.Std
  open Async.Std

  let main learn filename =
    let open NetKAT_LocalCompiler in
    let main () =
      let static = Async_NetKAT.create_from_file filename in
      let app = if learn
        then Async_NetKAT.union static (Learning.create ())
        else static
      in
      Async_NetKAT_Controller.start app () in
    never_returns (Scheduler.go_main ~max_num_open_file_descrs:4096 ~main ())
end

module Dump = struct
  open Core.Std
  open Async.Std

  type level = All | Policies | Flowtables | Stats

  let with_channel f chan =
    f (NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_channel chan))

  let with_file f filename =
    In_channel.with_file filename ~f:(with_channel f)

  let profile f =
    let t1 = Unix.gettimeofday () in
    let r = f () in
    let t2 = Unix.gettimeofday () in
    (t2 -. t1, r)

  module Local = struct

    let with_compile (sw : SDN_Types.switchId) (p : NetKAT_Types.policy) =
      let open NetKAT_LocalCompiler in
      let _ = 
        Format.printf "@[Compiling switch %Ld [size=%d]...@]%!"
          sw (NetKAT_Semantics.size p) in
      let c_time, i = profile (fun () -> compile sw p) in
      let t_time, t = profile (fun () -> to_table i) in
      let _ = Format.printf "@[Done [ctime=%fs ttime=%fs tsize=%d]@\n@]%!"
        c_time t_time (List.length t) in
      t

    let flowtable (sw : SDN_Types.switchId) t =
      if List.length t > 0 then
        Format.printf "@[flowtable for switch %Ld:@\n%a@\n@\n@]%!"
          sw
          SDN_Types.format_flowTable t

    let policy p =
      Format.printf "@[%a@\n@\n@]%!" NetKAT_Pretty.format_policy p

    let local f num_switches p =
      let rec loop switch_id =
        if switch_id > num_switches then ()
        else begin
          let swL = Int64.of_int32 switch_id in
          let sw_p = NetKAT_Types.(Seq(Filter(Test(Switch swL)), p)) in
          let t = with_compile swL sw_p in
          f swL t; loop Int32.(switch_id + 1l)
        end in
      loop 0l

    let all sw_num p =
      policy p;
      local flowtable sw_num p

    let stats sw_num p =
      local (fun x y -> ()) sw_num p

    let main level num_switches filename =
      match level with
        | All -> with_file (all num_switches) filename
        | Policies -> with_file policy filename
        | Flowtables -> with_file (local flowtable num_switches) filename
        | Stats -> with_file (stats num_switches) filename
  end
end

module Dexterize = struct
  open Decide_Ast
  open Decide_Ast.Term

  let header_value_to_pair h = 
    let open NetKAT_Types in 
    match h with
      | Switch(n) -> ("switch", Int64.to_string n)
      | Location(Physical n) -> ("port", Int32.to_string n)
      | Location(Pipe x) -> assert false
      | EthSrc(n) -> ("ethsrc", Int64.to_string n)
      | EthDst(n) -> ("ethdst", Int64.to_string n)
      | Vlan(n) -> ("vlan", string_of_int n)
      | VlanPcp(n) -> ("vlanpcp", string_of_int n)
      | EthType(n) -> ("ethtype", string_of_int n)
      | IPProto(n) -> ("ipproto", string_of_int n)
      | IP4Src(n,32l) -> ("ipsrc", Int32.to_string n)
      | IP4Dst(n,32l) -> ("ipdst", Int32.to_string n)
      | IP4Src(n,m) -> assert false
      | IP4Dst(n,m) -> assert false
      | TCPSrcPort(n) -> ("tcpsrcport", string_of_int n)
      | TCPDstPort(n) -> ("tcpdstport", string_of_int n)

  let rec pred_to_term = function
    | NetKAT_Types.True -> 
      One
    | NetKAT_Types.False -> 
      Zero
    | NetKAT_Types.Test(h) -> 
      let x,n = header_value_to_pair h in 
      Test(x,n)
    | NetKAT_Types.And(pr1,pr2) -> 
      Times[pred_to_term pr1; pred_to_term pr2]
    | NetKAT_Types.Or(pr1,pr2) -> 
      Plus (TermSet.add (pred_to_term pr1) (TermSet.singleton (pred_to_term pr2)))
    | NetKAT_Types.Neg(pr) -> 
      Not (pred_to_term pr) 

  let rec policy_to_term ?dup:(dup=true) = function
    | NetKAT_Types.Filter(p) -> 
      pred_to_term p
    | NetKAT_Types.Mod(h) -> 
      let x,n = header_value_to_pair h in 
      Assg(x,n)
    | NetKAT_Types.Union(p1,p2) -> 
      Plus (TermSet.add (policy_to_term ~dup:dup p1) (TermSet.singleton (policy_to_term ~dup:dup p2)))
    | NetKAT_Types.Seq(p1,p2) -> 
      Times[policy_to_term ~dup:dup p1; policy_to_term ~dup:dup p2]
    | NetKAT_Types.Star(p) -> 
      Star(policy_to_term ~dup:dup p)
    | NetKAT_Types.Link(sw1,pt1,sw2,pt2) -> 
      Times (Test("switch", Int64.to_string sw1) :: 
               Test("port", Int32.to_string pt1) ::
               Assg("switch", Int64.to_string sw2) :: 
               Assg("port", Int32.to_string pt2) ::
               if dup then [Dup] else [])
end

module Verify = 
struct
  module Node = Network_Common.Node
  module Link = Network_Common.Link 
  module Net = Network_Common.Net
  module Topology = Net.Topology
  module Path = Net.Path

  let is_host topo v = 
    match Node.device (Topology.vertex_to_label topo v) with 
      | Node.Host -> true 
      | _ -> false  
  let is_switch topo v = 
    match Node.device (Topology.vertex_to_label topo v) with 
      | Node.Switch -> true 
      | _ -> false  

  let topology filename = 
    let topo = Net.Parse.from_dotfile filename in 
    let vertexes = Topology.vertexes topo in 
    let hosts = Topology.VertexSet.filter (is_host topo) vertexes in 
    let switches = Topology.VertexSet.filter (is_switch topo) vertexes in 
    (topo, vertexes, switches, hosts)  

  let hosts_ports_switches topo hosts = 
    Topology.VertexSet.fold
      (fun h pol -> 
        let sw = Topology.VertexSet.choose (Topology.neighbors topo h) in 
        let _,pt = Topology.edge_dst (Topology.find_edge topo h sw) in 
        (h,pt,sw)::pol)
      hosts []

  let shortest_path_policy topo switches hosts = 
    Topology.VertexSet.fold
      (fun sw pol -> 
        let i = Node.id (Topology.vertex_to_label topo sw) in 
        Topology.VertexSet.fold
          (fun h pol -> 
            match Path.shortest_path topo sw h with 
              | Some(e::_) -> 
                let m = Node.mac (Topology.vertex_to_label topo h) in 
                let v,pt = Topology.edge_src e in 
                NetKAT_Types.(Union(pol,
                                    Seq(Filter(And(Test(Switch(i)),Test(EthDst(m)))),
                                        Mod(Location(Physical(pt))))))
              | _ -> pol)
          hosts pol)
      switches NetKAT_Types.drop 

  let shortest_path_table topo switches policy = 
    Topology.VertexSet.fold
      (fun sw tbl -> 
        let i = Node.id (Topology.vertex_to_label topo sw) in                     
        NetKAT_Types.(Union(tbl, Seq(Filter(Test(Switch(i))), 
                                     NetKAT_LocalCompiler.(to_netkat (compile i policy))))))
      switches NetKAT_Types.drop   

  let connectivity_policy topo hosts = 
    let hps = hosts_ports_switches topo hosts in 
    List.fold_left
      (fun (pr,pol) (h,pt,sw) -> 
        let m = Node.mac (Topology.vertex_to_label topo h) in               
        let i = Node.id (Topology.vertex_to_label topo sw) in 
        NetKAT_Types.(Or(pr,And(Test(Switch(i)), Test(Location(Physical(pt))))),
                      Union(pol, Seq(Seq(Filter(Test(EthDst(m))), Mod(Switch(i))), Mod(Location(Physical(pt)))))))
      NetKAT_Types.(False, id) hps
      
  let topology_policy topo = 
    Topology.EdgeSet.fold
      (fun e tp_pol -> 
        let v1,pt1 = Topology.edge_src e in 
        let v2,pt2 = Topology.edge_dst e in 
        if is_switch topo v1 && is_switch topo v2 then 
          let n1 = Node.id (Topology.vertex_to_label topo v1) in 
          let n2 = Node.id (Topology.vertex_to_label topo v2) in 
          NetKAT_Types.(Union(tp_pol, Link(n1,pt1,n2,pt2)))
        else tp_pol)
      (Topology.edges topo) NetKAT_Types.id 

  let verify_shortest_path_dup filename = 
    let topo, vertexes, switches, hosts = topology filename in 
    let sw_pol = shortest_path_policy topo switches hosts in 
    let sw_tbl = shortest_path_table topo switches sw_pol in 
    let tp_pol = topology_policy topo in 
    let net_sw_pol = NetKAT_Types.(Star(Seq(sw_pol, tp_pol))) in 
    let net_sw_tbl = NetKAT_Types.(Star(Seq(sw_tbl, tp_pol))) in 
    Printf.printf "## NetKAT Policy ##\n%s\n## OpenFlow Table ##\n%s\n## Topology ##\n%s\n%!"
      (NetKAT_Pretty.string_of_policy sw_pol)
      (NetKAT_Pretty.string_of_policy sw_tbl)
      (NetKAT_Pretty.string_of_policy tp_pol);
    Printf.printf "## Equivalent ##\n%b\n"
      (Decide_Bisimulation.check_equivalent
         (Dexterize.policy_to_term ~dup:true net_sw_pol)
         (Dexterize.policy_to_term ~dup:true net_sw_tbl))

  let verify_connectivity filename = 
    let topo, vertexes, switches, hosts = topology filename in 
    let sw_pol = shortest_path_policy topo switches hosts in 
    let cn_pr, cn_pol = connectivity_policy topo hosts in 
    let wrap pol = NetKAT_Types.(Seq(Seq(Filter cn_pr, pol), Filter cn_pr)) in 
    let tp_pol = topology_policy topo in 
    let net_sw_pol = wrap NetKAT_Types.(Star(Seq(sw_pol, tp_pol))) in 
    let net_cn_pol = wrap NetKAT_Types.(cn_pol) in 
    Printf.printf "## NetKAT Policy ##\n%s\n## Connectivity Policy ##\n%s\n%!"
      (NetKAT_Pretty.string_of_policy net_sw_pol)
      (NetKAT_Pretty.string_of_policy net_cn_pol);
    Printf.printf "## Equivalent ##\n%b\n"
      (Decide_Bisimulation.check_equivalent 
         (Dexterize.policy_to_term ~dup:false net_sw_pol)
         (Dexterize.policy_to_term ~dup:false net_cn_pol))

  let main = verify_connectivity
end

open Cmdliner

let policy n =
  let doc = "file containing a static NetKAT policy" in
  Arg.(required & (pos n (some file) None) & info [] ~docv:"FILE" ~doc)

let run_cmd : unit Cmdliner.Term.t * Cmdliner.Term.info =
  let learn =
    let doc = "enable per-switch L2 learning" in
    Arg.(value & flag & info ["learn"] ~doc)
  in
  let doc = "start a controller that will serve the static policy" in
  Term.(pure Run.main $ learn $ (policy 0)),
  Term.info "run" ~doc

let dump_cmd : unit Cmdliner.Term.t * Cmdliner.Term.info =
  let doc = "dump per-switch compiler results and statistics" in
  let switch_id =
    let doc = "the maximum switch id in the policy" in
    Arg.(required & (pos 0 (some int32) None) & info [] ~docv:"NUM_SWITCHES" ~doc)
  in
  let level =
    let doc = "Dump all compiler information (default)" in
    let all = Dump.All, Arg.info ["all"] ~doc in

    let doc = "Dump per-switch policy" in
    let policies = Dump.Policies, Arg.info ["policies"] ~doc in

    let doc = "Dump per-switch flowtables" in
    let flowtables = Dump.Flowtables, Arg.info ["flowtables"] ~doc in

    let doc = "Dump per-switch profiling statistics" in
    let stats = Dump.Stats, Arg.info ["stats"] ~doc in

    Arg.(last & vflag_all [Dump.All] [all;policies;flowtables;stats]) in
  Term.(pure Dump.Local.main $ level $ switch_id $ (policy 1)),
  Term.info "dump" ~doc

let verify_cmd : unit Cmdliner.Term.t * Cmdliner.Term.info = 
  let doc = "verify shortest-path forwarding compilation" in 
  let topo = 
    let doc = "the topology specified as a .dot file" in 
    Arg.(required & (pos 0 (some file) None) & info [] ~docv:"TOPOLOGY" ~doc)
  in 
  Term.(pure Verify.main $ topo), 
  Term.info "verify" ~doc

let default_cmd : unit Cmdliner.Term.t * Cmdliner.Term.info =
  let doc = "an sdn controller platform" in
  Term.(ret (pure (`Help(`Plain, None)))),
  Term.info "katnetic" ~version:"1.6.1" ~doc

let cmds = [run_cmd; dump_cmd; verify_cmd]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1 | _ -> exit 0
