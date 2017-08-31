open Core
open Frenetic_Nominal

(**
TODO:
* should really uses pipes instead of temporary files for interprocess
 communication; but performance is not an issue for now and files are easier
 and cross-plattform
*)

(*===========================================================================*)
(* hard-coded tests - just temporary for quick testin                        *)
(*===========================================================================*)

(* let () = begin
  let%nk barbell = {|
    filter port=1; port:=3; 1@3=>3@3; port:=1 +
    filter port=2; port:=3; 1@3=>3@3; port:=2
  |} in
  let%nk pol = {| filter switch = 1; port := 2 |} in
  Automaton.of_pol barbell
  |> Automaton.to_yojson
  |> printf "%a" (Yojson.Safe.pretty_to_channel ~std:false)
end *)

(*===========================================================================*)
(* main function                                                             *)
(*===========================================================================*)

(* takes NetKAT policy file; overwrites file with json representation of automaton *)
let main file =
  Frenetic_NetKAT_Parser.pol_of_file file
  |> Automaton.of_pol
  |> Automaton.to_yojson
  |> Yojson.Safe.to_file ~std:true file;
  exit 0


(* handle exceptions that may be thrown by main *)
let handle_main_exns f x =
  Frenetic_Util.pp_exceptions ();
  f x
  (* try f x with *)
  (* | _ -> failwith "todo" *)


(*===========================================================================*)
(* CMD Interface                                                             *)
(*===========================================================================*)

let cmd : Command.t =
  Command.basic
    ~summary:"Converts program to automaton and dumps it."
    (* ~readme: *)
    Command.Spec.(empty +> anon ("file" %: file))
    (fun file () -> handle_main_exns main file)

let () = Command.run ~version: "0.1" ~build_info:"" cmd
