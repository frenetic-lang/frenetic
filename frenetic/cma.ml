open Core.Std

let () =
  if Array.length Sys.argv < 3 then
    begin
      Printf.eprintf "usage: cma [pol] [sw]\n";
      exit 1
    end
  else
    Printf.printf "Invoking compiler with [%s] and [%s]\n%!" (Sys.argv.(1)) (Sys.argv.(2)); 
    let pol = Frenetic_NetKAT_Parser.policy_of_file (Sys.argv.(1)) in
    let sw = int_of_string (Sys.argv.(2)) in
    Printf.printf "Okay!\n%!"
