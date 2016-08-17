open Core.Std

module OF13 = Frenetic_OpenFlow0x04

let rec clean_iptables_list lst = 
  let sorted = List.rev (List.sort compare lst) in match sorted with
  (* gross hack for sometimes it generates a drop everything or id everything rule after specific filter rules *)
  | [] -> []
  | h::t -> if h = "iptables -A INPUT -j DROP" || h = "iptables -D INPUT -j DROP" then clean_iptables_list t else h::(clean_iptables_list t)
  
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
      (* Convert Fdd to iptables rules *)
      let tbl' = Frenetic_NetKAT_Compiler.to_iptables sw fdd in
      let tbl = if List.length tbl' = 1 then tbl' else clean_iptables_list tbl' in
      let () = Printf.eprintf "[cma] iptables is:\n%!" in
      let () = List.iter tbl ~f:(fun fm -> Printf.eprintf "  %s\n" fm) in
      let () = Printf.eprintf "\n%!" in
      (* execute the policy - this will vomit if you're not sudo *)
      let i = Sys.command "iptables -F" in
      let () = List.iter tbl ~f:(fun fm -> let n = Sys.command fm in ()) in
      (*let () = Printf.eprintf "[cma] table is:\n%!" in
      let () = List.iter tbl ~f:(fun fm -> Printf.eprintf "  %s\n" (OF13.FlowMod.to_string fm)) in
      let () = Printf.eprintf "\n%!" in    *)         
      (* fin. *)
      ()
    end
