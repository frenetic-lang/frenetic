open Core.Std

module OF13 = Frenetic_OpenFlow0x04

let () =
  if Array.length Sys.argv < 3 then
    begin
      Printf.eprintf "usage: cma [pol] [sw]\n";
      exit 1
    end
  else
    begin 
      let pol_fn = Sys.argv.(1) in
      let sw_str = Sys.argv.(2) in 
      let () = Printf.eprintf "[cma]: pol: [%s] sw: [%s]\n%!" pol_fn sw_str in 
      (* Convert command-line arguments *)
      let pol = Frenetic_NetKAT_Parser.policy_of_file pol_fn in 
      let sw = Int64.of_string sw_str in 
      let () = Printf.eprintf "[cma] policy is:\n%s\n%!" (Frenetic_NetKAT_Pretty.string_of_policy pol) in 
      (* Compile policy to Fdd *)
      let fdd = Frenetic_NetKAT_Compiler.compile_local pol in 
      let () = Printf.eprintf "[cma] fdd is:\n%s\n%!" (Frenetic_NetKAT_Compiler.to_string fdd) in 
      (* Convert Fdd to list of OF13 flowMods *)
      let tbl = Frenetic_NetKAT_Compiler.to_of13_table sw fdd in
      let () = Printf.eprintf "[cma] table is:\n%!" in
      let () = List.iter tbl ~f:(fun fm -> Printf.eprintf "  %s\n" (OF13.FlowMod.to_string fm)) in
      let () = Printf.eprintf "\n%!" in             
      (* fin. *)
      ()
    end
