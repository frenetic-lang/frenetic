(* Yes! This is user-requesting monitoring, so just print to stdout!!! *)
let printf = Printf.printf

let monitor_pol pol = 
  printf "policy is:\n%s\n" (NetCore_Types.Internal.pol_to_string pol);
  pol

let monitor_tbl sw pol = 
  let tbl = NetCore_Compiler.flow_table_of_policy sw pol in
  printf "Flow table at switch %Ld is:\n%!" sw;
  List.iter
    (fun (m,a) -> printf " %s => %s\n%!"
      (OpenFlow0x01.Match.to_string m)
      (OpenFlow0x01.Action.sequence_to_string a))
    tbl;
  pol
