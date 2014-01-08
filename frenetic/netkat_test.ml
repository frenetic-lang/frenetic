(*
open NetKAT_Parser
open NetKAT_Types

let netkat_of_string (s: string) : NetKAT_Types.policy = 
  program NetKAT_Lexer.token (Lexing.from_string s)

let rec repl () : unit =
  print_string "> ";
  (
  try
    print_string (string_of_policy (netkat_of_string (read_line ())))
  with _ -> print_string "Oops, parse error"
  );
  print_newline ();
  repl ()
   
let _  =
  repl ()
*)



module Example = struct
  open SDN_Types
  open VInt
  open Types

  (* End to end connectivity via mac addresses routing, hard coded for a tree topo using 4 nodes *)

  let match_switch (sw : int) : string =
    "(filter (switch = " ^ (string_of_int sw) ^ "))"
  
  
  let match_dst_mac (dst_mac: string) : string =
    "(filter (dlDst = " ^ dst_mac ^ "))"
  

  let mod_out_port (out_port : int) : string =
    "(inPort -> " ^ (string_of_int out_port) ^ ")"

  let out_rule (dst_info : string * int) : string =
    match dst_info with
      | (dst_mac, out_port) -> 
          "(" ^ (match_dst_mac dst_mac) ^ ";" ^ (mod_out_port out_port) ^ ")"
   

  (* (match_switch sw);  *)
  let switch_policy (sw : int) (dsts : (string * int) list) : string =
    let sw_pol = String.concat " + " (List.map out_rule dsts) in
    "(" ^ (match_switch sw) ^ ";" ^ "(" ^ sw_pol ^ "))"

  let pol_switch1 = switch_policy 1 [("00:00:00:00:00:01", 1);
                                     ("00:00:00:00:00:02", 2);
                                     ("00:00:00:00:00:03", 3);
                                     ("00:00:00:00:00:04", 3);]

  let pol_switch2 = switch_policy 2 [("00:00:00:00:00:01", 1);
                                     ("00:00:00:00:00:02", 1);
                                     ("00:00:00:00:00:03", 2);
                                     ("00:00:00:00:00:04", 2);]

  let pol_switch3 = switch_policy 3 [("00:00:00:00:00:01", 1);
                                     ("00:00:00:00:00:02", 1);
                                     ("00:00:00:00:00:03", 2);
                                     ("00:00:00:00:00:04", 3);]

  let pol_str = pol_switch1 ^ " + " ^ pol_switch2 ^ " + " ^ pol_switch3




end

let () =
  let local p =
    (fun sw -> LocalCompiler.to_table
      (LocalCompiler.compile sw p)) in
  print_string (Example.pol_str ^ "\n\n");
  let pol = Parser.program Lexer.token (Lexing.from_string Example.pol_str) in
  Lwt_main.run (Controller.start local 6633 (NetKAT_Stream.constant pol))
