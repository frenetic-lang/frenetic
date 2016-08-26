open Core.Std
open Frenetic_NetKAT
open Benchmarking_Util

let to_table = Frenetic_NetKAT_Compiler.to_table

let print_flowtables tbls =
  List.iter tbls ~f:(fun tbl ->
    Frenetic_OpenFlow.format_flowTable Format.std_formatter tbl;
    print_newline ())

let dst_based_routing ~(in_file : string) ~(out_file : string) ~kind : unit =
  printf "Parsing topology file...\n%!";
  let topo = PolicyGen.parse_topo_file in_file in
  printf "Generating policy...\n%!";
  let pol = match kind with
    | `Local -> PolicyGen.shortest_paths topo
    | `Global -> PolicyGen.shortest_paths_global_policy topo in
  printf "Dumping policy to JSON...\n%!";
  let str = Frenetic_NetKAT_Json.policy_to_json_string pol in
  Out_channel.with_file out_file ~f:(fun chan ->
    Out_channel.output_string chan str)

let fattree_json ~(k : int) ~(out_file : string) : unit =
  Out_channel.with_file out_file ~f:(fun chan ->
    FatTree.fattree_routing ~k
    |> Frenetic_NetKAT_Json.policy_to_json_string
    |> Out_channel.output_string chan)

let pretty_json filename =
  let pol = In_channel.with_file filename
    ~f:Frenetic_NetKAT_Json.policy_of_json_channel
    |> Frenetic_NetKAT_Pretty.pretty_assoc
  in
  print_endline (Frenetic_NetKAT_Pretty.string_of_policy pol)

let classbench ~(in_file : string) ~(out_file : string) : unit =
  let pred = ClassBench.parse_classbench in_file in
  let str = Frenetic_NetKAT_Json.policy_to_json_string (Filter pred) in
  Out_channel.with_file out_file ~f:(fun chan ->
    Out_channel.output_string chan str)

let switches pol = match Frenetic_NetKAT_Semantics.switches_of_policy pol with
    | [] -> [1L]
    | n -> n

let per_switch_compilation to_table compiler pol : int =
  let switches = switches pol in
  let switches_and_pols = List.map switches
    ~f:(fun sw -> Frenetic_NetKAT_Optimize.specialize_policy sw pol) in
  let tbl_lens = List.map switches_and_pols
    ~f:(fun pol -> List.length (to_table 0L (compiler pol))) in
  int_sum tbl_lens

let big_fdd_compilation to_table compiler pol : int =
  let switches = switches pol in
  let fdd = compiler pol in
  let tbl_lens = List.map switches
    ~f:(fun sw -> List.length (to_table sw fdd)) in
  int_sum tbl_lens

let print_table to_table sw fdd =
  let tbl = to_table sw fdd in
  let sw_str = Int64.to_string sw in
  print_endline (Frenetic_OpenFlow.string_of_flowTable ~label:sw_str tbl);
  tbl

let compile compiler per_switch varorder tbl_opt debug filename =
  let is_debug = match debug with
    | "true" -> true
    | "false" -> false
    | _ -> assert false in
  let order =
    let open Frenetic_NetKAT_Compiler.Field in
    match varorder with
    | "varorder-heuristic" -> `Heuristic
    | "varorder-fattree" ->
      `Static [ EthType; Switch; Location; EthSrc; EthDst; Vlan;
                VlanPcp; IPProto;IP4Src; IP4Dst; TCPSrcPort; TCPDstPort; VSwitch; VPort; VFabric;
                Meta0; Meta1; Meta2; Meta3; Meta4; ]
    | "varorder-zoo" ->
      `Static [ EthType; Switch; IP4Dst; Location; EthSrc; EthDst; Vlan;
                VlanPcp; IPProto;IP4Src; TCPSrcPort; TCPDstPort; VSwitch; VPort; VFabric;
                Meta0; Meta1; Meta2; Meta3; Meta4; ]
    | _ -> assert false in
  let to_table sw fdd = match tbl_opt with
    | "tablegen-steffen" ->
       Frenetic_NetKAT_Compiler.(to_table ~options:{default_compiler_options with optimize=true; dedup_flows=true } sw fdd)
    | "tablegen-naive" ->
       Frenetic_NetKAT_Compiler.(to_table ~options:{default_compiler_options with optimize=false; dedup_flows=true } sw fdd)
    | _ -> assert false in
  let to_table sw fdd = match is_debug with
    | false -> to_table sw fdd
    | true -> print_table to_table sw fdd in
  let compiler_fun = match compiler with
    | "global" ->
       (fun pol ->
	let fdd = Frenetic_NetKAT_Compiler.compile_global pol in
        (if not is_debug then
           Frenetic_NetKAT_Compiler.to_dotfile fdd "fdk.dot");
	fdd)
    | "local" ->
       let opts =
	 {Frenetic_NetKAT_Compiler.default_compiler_options with
	   field_order=order;
	   cache_prepare=`Empty} in
       Frenetic_NetKAT_Compiler.compile_local opts
    | _ -> assert false in
  let f = match per_switch with
    | "big-fdd" -> big_fdd_compilation to_table compiler_fun
    | "per-switch" -> per_switch_compilation to_table compiler_fun
    | _ -> assert false in
  let pol = In_channel.with_file filename
    ~f:Frenetic_NetKAT_Json.policy_of_json_channel in
  let (compile_time, tbl_size) = profile (fun () -> f pol) in
  printf "%s,%s,%s,%s,%s,%d,%f\n" filename compiler per_switch varorder tbl_opt tbl_size compile_time

let policy_size filename =
  let pol = In_channel.with_file filename
    ~f:Frenetic_NetKAT_Json.policy_of_json_channel in
  let size = Frenetic_NetKAT_Semantics.size pol in
  printf "%s,%d\n" filename size


let sdx filename =
  let json = In_channel.with_file filename ~f:Yojson.Basic.from_channel in
  let pols =
    let open Yojson.Basic.Util in
    assert (json |> member "type" |> to_string = "disjoint");
    json |> member "pols" |> to_list
      |> List.map ~f:Frenetic_NetKAT_Json.policy_of_json in
  let open Frenetic_NetKAT_Pretty in
  (* let _ = List.iteri pols ~f:(fun i pol -> *)
    (* string_of_policy pol |> printf "Policy %d:\n%s\n\n%!" i) in *)
  let order =
    let open Frenetic_NetKAT_Compiler.Field in
    `Static [ Location; EthDst; TCPSrcPort; TCPDstPort; IP4Src; EthType; Switch; IP4Dst;
              EthSrc;  Vlan; VlanPcp; IPProto; VSwitch; VPort; VFabric; Meta0; Meta1; Meta2; Meta3; Meta4 ] in
  (* eprintf "Number of elements in disjoint union: %d\n%!" (List.length pols); *)
  let f pol =
    let opts = { Frenetic_NetKAT_Compiler.default_compiler_options with
		 field_order = order;
		 optimize = true } in
    let tbl = Frenetic_NetKAT_Compiler.to_table ~options:opts 0L
		(Frenetic_NetKAT_Compiler.compile_local opts pol) in
     (* eprintf "Table:\n%s\n%!" (Frenetic_OpenFlow.string_of_flowTable tbl); *)
    List.length tbl in
  let (compile_time, tbl_size) = profile (fun () ->
    int_sum (List.map pols ~f) - List.length pols) in
  printf "%s,%d,%f\n%!" filename tbl_size compile_time

let dot_to_virtual ~in_file =
  let topo = PolicyGen.parse_topo_file ~log:false in_file in
  let (vpol, vrel, vtopo, vingpol, vinout, ptopo, pinout) = PolicyGen.big_switch ~topo in
  let open Frenetic_NetKAT_Pretty in
  let open Out_channel in
  let () = () in
  List.iter [("vpol", vpol); ("vtopo", vtopo); ("ptopo", ptopo)]
    ~f:(fun (file, pol) -> write_all (file ^ ".kat") ~data:(string_of_policy pol));
  List.iter [("vrel", vrel); ("vinout", vinout); ("pinout", pinout)]
    ~f:(fun (file, pred) -> write_all (file ^ ".kat") ~data:(string_of_pred pred))


let _  = match Array.to_list Sys.argv with
  (* Run and benchmark the compiler.

     The debug flag is not implemented, but it is intended to dump flow tables
     and the FDD.

     The rest argument is an optional string that is ignored. It makes it
     easy to write bad gnu-parallel scripts (i.e., rest could be the index of
     the trial.)
   *)
  | _ :: "compile" :: compiler :: per_switch :: varoder :: tblgen :: debug :: filename :: rest ->
    assert (List.length rest <= 1);
    compile compiler per_switch varoder tblgen debug filename
  | _ :: "compile" :: "sdx" :: filename :: rest ->
    assert (List.length rest <= 1);
    sdx filename
  (* Converts a classbench classifier into a NetKAT filter.

    The output is simply a filter-policy. *)
  | [ _; "classbench"; in_file; out_file ] -> classbench in_file out_file
  (* Generates a destination-based routing policy for the provided topology.

     This policy can be compiled with the local-compiler.
   *)
  | [ _; "dot-to-json"; in_file; out_file ] ->
    dst_based_routing ~in_file ~out_file ~kind:`Local
  | [ _; "global-routing"; in_file; out_file ] ->
    dst_based_routing ~in_file ~out_file ~kind:`Global
  | [ _; "dot-to-virtual"; in_file ] ->
    dot_to_virtual ~in_file

  (* Generates a routing policy for a k-pod fat-tree.

    This policy can be compiled with the local-compiler.
   *)
  | [ _; "fattree-json"; k; out_file ] ->
    let k = Int.of_string k in
    fattree_json ~k ~out_file
  | [_; "pretty-json"; filename ] -> pretty_json filename
  | [_; "policy-size"; filename ] -> policy_size filename
  | _ ->
    printf "Invalid command line arguments. Read source code for help.\n"
