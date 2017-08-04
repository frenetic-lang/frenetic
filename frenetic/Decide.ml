open Core
open Dump


(*===========================================================================*)
(* COMMANDS                                                                  *)
(*===========================================================================*)


module Main = struct
  let spec = Command.Spec.(
    empty
    +> anon ("file" %: file)
  )

  let run file () =
    let equalities = Frenetic_NetKAT_Parser.equalities_of_file file in
    List.iter equalities ~f:(fun (p,q) ->
      let open Frenetic_NetKAT_Pretty in
      printf "===================================================\n";
      printf "(%s)  ==  (%s)\n\n" (string_of_policy p) (string_of_policy q);
      let (ta, (a1, a2)) = Dump.time (fun () ->
        Frenetic_NetKAT_Compiler.Automaton.(of_pol p, of_pol q)) in
      Dump.print_time ~prefix:"automata construction " ta;
      let (th, rh) = Dump.time (fun () ->
        Frenetic_NetKAT_Equivalence.Hopcroft.equiv a1 a2) in
      printf "\nHopcroft: %s\n" (Bool.to_string rh);
      Dump.print_time th;
      let (ts, rs) = Dump.time (fun () ->
        Frenetic_NetKAT_Equivalence.Simple.equiv a1 a2) in
      printf "\nSimple: %s\n" (Bool.to_string rs);
      Dump.print_time ts)

end



(*===========================================================================*)
(* BASIC SPECIFICATION OF COMMANDS                                           *)
(*===========================================================================*)


let main : Command.t =
  Command.basic
    ~summary:"Parses file of NetKAT (in-)equalities and decides them."
    (* ~readme: *)
    Main.spec
    Main.run
