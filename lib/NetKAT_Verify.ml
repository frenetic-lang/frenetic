module Dexterize = struct
  open Decide_Ast
  open Decide_Ast.Term
  module Value = Decide_Util.Value
  module Field = Decide_Util.Field

  let header_value_to_pair h = 
    let open NetKAT_Types in 
    match h with
      | Switch(n) -> ("switch", Int64.to_string n)
      | Location(Physical n) -> ("port", Int32.to_string n)
      | Location(Pipe x) -> ("port", x)
      | EthSrc(n) -> ("ethsrc", Int64.to_string n)
      | EthDst(n) -> ("ethdst", Int64.to_string n)
      | Vlan(n) -> ("vlan", string_of_int n)
      | VlanPcp(n) -> ("vlanpcp", string_of_int n)
      | EthType(n) -> ("ethtype", string_of_int n)
      | IPProto(n) -> ("ipproto", string_of_int n)
      | IP4Src(n,32l) -> ("ipsrc", Int32.to_string n)
      | IP4Dst(n,32l) -> ("ipdst", Int32.to_string n)
      | IP4Src(n,m) -> ("ipsrc", Int32.to_string n)
      | IP4Dst(n,m) -> ("ipdst", Int32.to_string n)
      | TCPSrcPort(n) -> ("tcpsrcport", string_of_int n)
      | TCPDstPort(n) -> ("tcpdstport", string_of_int n)

  let rec pred_to_term = function
    | NetKAT_Types.True -> 
      make_one ()
    | NetKAT_Types.False -> 
      make_zero ()
    | NetKAT_Types.Test(h) -> 
      let x,n = header_value_to_pair h in 
      make_test (Field.of_string x, Value.of_string n)
    | NetKAT_Types.And(pr1,pr2) -> 
      make_times [pred_to_term pr1; pred_to_term pr2]
    | NetKAT_Types.Or(pr1,pr2) -> 
      make_plus (Decide_Ast.TermSet.of_list [(pred_to_term pr1);(pred_to_term pr2)])
    | NetKAT_Types.Neg(pr) -> 
      make_not (pred_to_term pr) 

  let rec policy_to_term ?dup:(dup=true) = function
    | NetKAT_Types.Filter(p) -> 
      pred_to_term p
    | NetKAT_Types.Mod(h) -> 
      let x,n = header_value_to_pair h in 
      make_assg (Field.of_string x, Value.of_string n)
    | NetKAT_Types.Union(p1,p2) -> 
      make_plus (Decide_Ast.TermSet.of_list [policy_to_term ~dup:dup p1; policy_to_term ~dup:dup p2])
    | NetKAT_Types.Seq(p1,p2) -> 
      make_times [policy_to_term ~dup:dup p1; policy_to_term ~dup:dup p2]
    | NetKAT_Types.Star(p) -> 
      make_star (policy_to_term ~dup:dup p)
    | NetKAT_Types.Link(sw1,pt1,sw2,pt2) -> 
      make_times (make_test (Field.of_string "switch", Value.of_string (Int64.to_string sw1)) :: 
               make_test (Field.of_string "port", Value.of_string (Int32.to_string pt1)) ::
               make_assg (Field.of_string "switch", Value.of_string (Int64.to_string sw2)) :: 
               make_assg (Field.of_string "port", Value.of_string (Int32.to_string pt2)) ::
               if dup then [make_dup ()] else [])
end

module IPMasks = struct

  module IPMask = struct
    type t = int32 * int32
    let compare = compare
  end
  module IP = struct
    type t = int32
    let compare = compare
  end
  module IPMaskSet = Set.Make(IPMask)
  module IPMaskMap = Map.Make(IPMask)
  module IPSet = Set.Make(IP)
  module BrxMap = Map.Make(Brx)

  let brx_of_ip (p,m) = 
    let open Brx in 
    let m = Int32.to_int m in 
    let zero = mk_cset [(0,0)] in 
    let one = mk_cset [(1,1)] in 
    let any = mk_cset [(0,1)] in 
    let rec loop x n acc = 
      if n = 0 then acc
      else
	let acc' = 
	  if Int32.rem x 2l = 0l then mk_seq zero acc
	  else mk_seq one acc in 
	let n' = pred n in 
	let x' = Int32.shift_right_logical x 1 in 
	loop x' n' acc'  in 
    loop p m (mk_iter any (32 - m) (32 - m))

  let any_ip = brx_of_ip (0l,0l)

  let ip_of_brx b = 
    match Brx.representative b with 
    | None -> 
      None
    | Some s -> 
      let n = String.length s in 
      let rec loop i acc = 
	if i >= n then acc
	else
	  begin 
	    match Char.code s.[i] with
	    | 0 -> 
	      loop (succ i) (Int32.shift_left acc 1)
	    | 1 ->
	      loop (succ i) (Int32.add (Int32.shift_left acc 1) 1l)
	    | c -> 
	      failwith (Printf.sprintf "invalid regular expression: %d" c)
	  end in 
      Some (loop 0 0l)
	
  let rec ips_of_pred = function
    | NetKAT_Types.True -> 
      IPMaskSet.empty
    | NetKAT_Types.False -> 
      IPMaskSet.empty
    | NetKAT_Types.Test(NetKAT_Types.IP4Src(p,m)) -> 
      IPMaskSet.singleton (p,m)
    | NetKAT_Types.Test(NetKAT_Types.IP4Dst(p,m)) -> 
      IPMaskSet.singleton (p,m)
    | NetKAT_Types.Test(_) -> 
      IPMaskSet.empty
    | NetKAT_Types.And(pr1,pr2) -> 
      IPMaskSet.union (ips_of_pred pr1) (ips_of_pred pr2)
    | NetKAT_Types.Or(pr1,pr2) -> 
      IPMaskSet.union (ips_of_pred pr1) (ips_of_pred pr2)
    | NetKAT_Types.Neg(pr) -> 
      ips_of_pred pr
  let rec ips_of_policy = function
    | NetKAT_Types.Filter(pr) -> 
      ips_of_pred pr
    | NetKAT_Types.Mod(NetKAT_Types.IP4Src(p,m)) -> 
      IPMaskSet.singleton (p,m)
    | NetKAT_Types.Mod(NetKAT_Types.IP4Dst(p,m)) -> 
      IPMaskSet.singleton (p,m)
    | NetKAT_Types.Mod(_) -> 
      IPMaskSet.empty
    | NetKAT_Types.Union(p1,p2) -> 
      IPMaskSet.union (ips_of_policy p1) (ips_of_policy p2)
    | NetKAT_Types.Seq(p1,p2) -> 
      IPMaskSet.union (ips_of_policy p1) (ips_of_policy p2)
    | NetKAT_Types.Star(p) -> 
      ips_of_policy p
    | NetKAT_Types.Link(sw1,pt1,sw2,pt2) -> 
      IPMaskSet.empty

  let partition_ips ips = 
    let part = 
      IPMaskSet.fold
	(fun x acc -> 
	  let bx = brx_of_ip x in 
	  let sx = IPMaskSet.singleton x in 
	  BrxMap.fold
	    (fun by s acc -> 
	      let l = Brx.mk_diff bx by in 
	      let i = Brx.mk_inter bx by in 
	      let r = Brx.mk_diff by bx in 
	      let accl = 
		if Brx.is_empty l then acc 
		else BrxMap.add l sx acc in 
	      let acci = 
		if Brx.is_empty i then accl 
		else BrxMap.add i (IPMaskSet.union s sx) accl in
	      let accr = 
		if Brx.is_empty r then acci 
		else BrxMap.add r s accl in 
	      accr)
	    acc BrxMap.empty)
	ips 
	(BrxMap.singleton any_ip IPMaskSet.empty) in 
    BrxMap.fold
      (fun bx s acc -> 
	match ip_of_brx bx with 
	| None -> acc
	| Some a -> 
	  let sa = IPSet.singleton a in 
	  IPMaskSet.fold
	    (fun y acc -> 
	      let sy = 
		try IPMaskMap.find y acc
		with Not_found -> IPSet.empty in 
	      IPMaskMap.add y (IPSet.union sa sy) acc)
	    s acc)
      part IPMaskMap.empty

  let rec subst_pred subst pr = match pr with 
    | NetKAT_Types.True -> 
      pr
    | NetKAT_Types.False -> 
      pr
    | NetKAT_Types.Test(NetKAT_Types.IP4Src(p,m)) -> 
      NetKAT_Types.(try 
	 IPSet.fold 
	   (fun a acc -> Optimize.mk_or (Test(IP4Src(a,32l))) acc)
	   (IPMaskMap.find (p,m) subst)
	   (False)
       with Not_found -> 
	 failwith "subst_pred")
    | NetKAT_Types.Test(NetKAT_Types.IP4Dst(p,m)) -> 
      NetKAT_Types.(try 
	 IPSet.fold 
	   (fun a acc -> Optimize.mk_or (Test(IP4Dst(a,32l))) acc)
	   (IPMaskMap.find (p,m) subst)
	   (False)
       with Not_found -> 
	 failwith "subst_pred")
    | NetKAT_Types.Test(_) -> 
      pr
    | NetKAT_Types.And(pr1,pr2) -> 
      NetKAT_Types.And(subst_pred subst pr1, subst_pred subst pr2)
    | NetKAT_Types.Or(pr1,pr2) -> 
      NetKAT_Types.Or(subst_pred subst pr1, subst_pred subst pr2)
    | NetKAT_Types.Neg(pr) -> 
      NetKAT_Types.Neg(subst_pred subst pr)
  let rec subst_policy subst pol = match pol with 
    | NetKAT_Types.Filter(pr) -> 
      NetKAT_Types.Filter(subst_pred subst pr)
    | NetKAT_Types.Mod(NetKAT_Types.IP4Src(p,m)) -> 
      pol (* should be identical to what's in subst *)
    | NetKAT_Types.Mod(NetKAT_Types.IP4Dst(p,m)) -> 
      pol (* should be identical to what's in subst *)
    | NetKAT_Types.Mod(_) -> 
      pol
    | NetKAT_Types.Union(p1,p2) -> 
      NetKAT_Types.Union(subst_policy subst p1, subst_policy subst p2)
    | NetKAT_Types.Seq(p1,p2) -> 
      NetKAT_Types.Seq(subst_policy subst p1, subst_policy subst p2)
    | NetKAT_Types.Star(p) -> 
      NetKAT_Types.Star(subst_policy subst p)
    | NetKAT_Types.Link(sw1,pt1,sw2,pt2) -> 
      pol
	
  let skolemize pol = 
    let ips = ips_of_policy pol in 
    let subst = partition_ips ips in 
    let pol' = subst_policy subst pol in 
    pol'
end

module Verify = 
struct
  module Node = Network_Common.Node
  module Link = Network_Common.Link 
  module Net = Network_Common.Net
  module Topology = Net.Topology
  module Path = Net.Path

  module Json = Yojson.Safe

  let is_host topo v = 
    match Node.device (Topology.vertex_to_label topo v) with 
      | Node.Host -> true 
      | _ -> false  
  let is_switch topo v = 
    match Node.device (Topology.vertex_to_label topo v) with 
      | Node.Switch -> true 
      | _ -> false  

  let stanford_json filename = 
    let parse_rule = function
      | `Assoc [("in_ports", `List in_ports); 
		("ip_dst_wc", ip_dst_wc); 
		("ip_dst_new", ip_dst_new);
		("out_ports", `List out_ports);
		("ip_dst_match", ip_dst_match)] -> 
	let ip_pat = 
	  match ip_dst_match, ip_dst_wc with 
	  | `Int p, `Int m -> 
	    NetKAT_Types.(Optimize.(mk_filter (Test(IP4Dst(Int32.of_int p, Int32.of_int m)))))
	  | j1,j2 -> 
	    failwith (Printf.sprintf "bad ip_dst_match or ip_dst_wc: %s %s" (Json.to_string j1) (Json.to_string j2)) in 
	let ip_mod = 
	  match ip_dst_new with 
	  | `Int n -> 
	    NetKAT_Types.(Optimize.(Mod(IP4Dst(Int32.of_int n,32l))))
	  | `Null -> 
	    NetKAT_Types.(Optimize.(mk_filter True))
	  | j -> 
	    failwith (Printf.sprintf "bad ip_dst_new: %s" (Json.to_string j)) in 
	let outs = 
	  List.fold_left  
	    (fun acc outp -> 
	      match outp with 
	      | `Int n -> 
		NetKAT_Types.(Optimize.(mk_union acc (Mod(Location(Physical(Int32.of_int n))))))
	      | j -> 
		failwith (Printf.sprintf "bad out_port: %s" (Json.to_string j)))
	    NetKAT_Types.(Optimize.(mk_filter False)) out_ports in
	let acts = NetKAT_Types.(Optimize.(mk_seq ip_mod outs)) in 
	let pol = 
	  List.fold_left
	    (fun acc inp -> 
	      match inp with 
	      | `Int n -> 
		let pr = NetKAT_Types.(Optimize.(mk_seq (mk_filter(Test(Location(Physical(Int32.of_int n))))) ip_pat)) in 
		NetKAT_Types.(Optimize.(mk_union acc (mk_seq pr acts)))
	      | j -> 
		failwith (Printf.sprintf "bad in_port: %s" (Json.to_string j)))
	    NetKAT_Types.(Optimize.(mk_filter False)) in_ports in 
	pol
      | j -> 
	failwith (Printf.sprintf "bad_rule: %s" (Json.to_string j)) in 
    let parse_file = function
      | `Assoc ["rules", `List rules] -> 
	List.fold_left
	  (fun acc rule -> 
	    NetKAT_Types.(Optimize.(mk_union acc (parse_rule rule))))
	  NetKAT_Types.(Optimize.(mk_filter False)) rules
      | j -> 
	failwith (Printf.sprintf "bad file: %s" (Json.to_string j)) in 
    parse_file (Json.from_file filename)

  let () = Printf.printf "%s" (NetKAT_Pretty.string_of_policy (stanford_json "foo.of"))

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

  let check_equivalent t1 t2 = 
    let module UnivMap = Decide_Util.SetMapF (Decide_Util.Field) (Decide_Util.Value) in
    let t1vals = Decide_Ast.Term.values t1 in 
    let t2vals = Decide_Ast.Term.values t2 in 
    let ret = if ((not (UnivMap.is_empty t1vals)) || (not (UnivMap.is_empty t2vals)))
      then 
	begin 
	  let univ = UnivMap.union t1vals t2vals in 
	  let univ = List.fold_left 
	    (fun u x -> UnivMap.add x Decide_Util.Value.extra_val u) univ (UnivMap.keys univ) in
	  let module UnivDescr = struct
	    let all_fields : Decide_Util.FieldSet.t = 
	      (* TODO: fix me when SSM is eliminated *)
	      List.fold_right 
		(fun f -> 
		  Decide_Util.FieldSet.add f) (UnivMap.keys univ) Decide_Util.FieldSet.empty
	    let _ = assert (Decide_Util.FieldSet.cardinal all_fields > 0 )
	    let all_values f : Decide_Util.ValueSet.t = 
	      try 
		UnivMap.Values.fold (fun v acc -> Decide_Util.ValueSet.add v acc ) (UnivMap.find_all f univ) 
		  Decide_Util.ValueSet.empty
	      with Not_found -> 
		Decide_Util.ValueSet.empty
	  end in   
	  Decide_Util.all_fields := (fun _ -> UnivDescr.all_fields);
	  Decide_Util.all_values := (fun _ -> UnivDescr.all_values);
	  Decide_Bisimulation.check_equivalent t1 t2
	end      
      else (
	Decide_Ast.Term.equal t1 t2) in 
    ret


  let verify_shortest_paths ?(print=true) filename = 
    let topo, vertexes, switches, hosts = topology filename in 
    let sw_pol = shortest_path_policy topo switches hosts in 
    let edge_pol, _ = connectivity_policy topo hosts in 
    let wrap pol = NetKAT_Types.(Seq(Seq(Filter edge_pol, pol), Filter edge_pol)) in 
    let sw_tbl = shortest_path_table topo switches sw_pol in 
    let tp_pol = topology_policy topo in 
    let net_sw_pol = NetKAT_Types.(Star(Seq(sw_pol, tp_pol))) in 
    let net_sw_tbl = NetKAT_Types.(Star(Seq(sw_tbl, tp_pol))) in 
    if print then Printf.printf "## NetKAT Policy ##\n%s\n## OpenFlow Table ##\n%s\n## Topology ##\n%s\n%!"
      (NetKAT_Pretty.string_of_policy sw_pol)
      (NetKAT_Pretty.string_of_policy sw_tbl)
      (NetKAT_Pretty.string_of_policy tp_pol);
    let dl = (Dexterize.policy_to_term ~dup:true (wrap net_sw_pol)) in 
    let dr = (Dexterize.policy_to_term ~dup:true (wrap net_sw_tbl)) in
    if print then Printf.printf "## Dexter Policy: ##\n%s\n##Dexter OF policy: ##\n%s\n"
      (Decide_Ast.Term.to_string dl)
      (Decide_Ast.Term.to_string dr);
    let ret = (check_equivalent dl dr) in 
    if print then Printf.printf "## Equivalent ##\n%b\n" ret;
    ret
      


  let verify_connectivity ?(print=true) filename = 
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
    let lhs = Dexterize.policy_to_term ~dup:false net_sw_pol in 
    let rhs = Dexterize.policy_to_term ~dup:false net_cn_pol in 
    Printf.printf "## Dexter NetKAT Policy ##\n%s\n## Dexter Connectivity Policy ##\n%s\n%!"
      (Decide_Ast.Term.to_string lhs)
      (Decide_Ast.Term.to_string rhs);
    Printf.printf "## Equivalent ##\n%b\n"
      (check_equivalent lhs rhs)


end
