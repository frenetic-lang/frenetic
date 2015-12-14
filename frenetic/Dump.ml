open Core.Std

(*===========================================================================*)
(* UTILITY FUNCTIONS                                                         *)
(*===========================================================================*)

let parse_pol file =
  In_channel.read_all file
  |> Frenetic_NetKAT_Parser.policy_from_string

let parse_pred file =
  In_channel.read_all file
  |> Frenetic_NetKAT_Parser.pred_from_string

let dump_fdd fdd =
  printf "%s\n" (Frenetic_NetKAT_Compiler.to_string fdd)

let dump_table fdd sw =
  Frenetic_NetKAT_Compiler.to_table sw fdd
  |> Frenetic_OpenFlow.string_of_flowTable ~label:(sprintf "Switch %Ld" sw)
  |> printf "%s\n"

let dump_all_tables switches fdd =
  List.iter switches ~f:(dump_table fdd)




(*===========================================================================*)
(* COMMANDS                                                                  *)
(*===========================================================================*)

module Local = struct
  let spec = Command.Spec.(
    empty
    +> anon ("filename" %: file)
    +> flag "--switches" (optional int)
        ~doc:"n number of switches to dump flow tables for \
               (assuming switch-numbering 1,2,...,n)"
    +> flag "--dump-fdd" no_arg
        ~doc:" dump the intermediate representation generated \
              by the local compiler"
  )

  let run file nr_switches dumpfdd () =
    let pol = parse_pol file in
    let fdd = Frenetic_NetKAT_Compiler.compile_local pol in
    let switches = match nr_switches with
      | None -> Frenetic_NetKAT_Semantics.switches_of_policy pol
      | Some n -> List.range 0 n |> List.map ~f:Int64.of_int
    in
    if Option.is_none nr_switches && List.is_empty switches then
      printf "Number of switches not automatically recognized!\n\
              Use the --switch flag to specify it manually.\n"
    else
      if dumpfdd then dump_fdd fdd;
      dump_all_tables switches fdd
end



module Global = struct
  let spec = Command.Spec.(
    empty
    +> anon ("filename" %: file)
  )

  let run file () =
    let pol = parse_pol file in
    let fdd = Frenetic_NetKAT_Compiler.compile_global pol in
    let switches = Frenetic_NetKAT_Semantics.switches_of_policy pol in
    dump_all_tables switches fdd
end



module Virtual = struct
  let spec = Command.Spec.(
    empty
    +> flag "--vpolicy" (optional_with_default "vpolicy.dot" file) ~doc: "File containing local virtual policy (containing no links)"
    +> flag "--vrel" (optional_with_default "vrel.kat" file) ~doc: "File containing virtual relation"
    +> flag "--vtopo" (optional_with_default "vtopo.kat" file) ~doc: "File containing virtual topology"
    +> flag "--ving-pol" (optional_with_default "ving_pol.kat" file) ~doc: "File containing virtual ingress policy"
    +> flag "--ving" (optional_with_default "ving.kat" file) ~doc: "File containing virtual ingress predicate"
    +> flag "--veg" (optional_with_default "veg.kat" file) ~doc: "File containing virtual egress predicate"
    +> flag "--ptopo" (optional_with_default "ptopo.kat" file) ~doc: "File containing physical topology"
    +> flag "--ping" (optional_with_default "ping.kat" file) ~doc: "File containing physical ingress"
    +> flag "--peg" (optional_with_default "peg.kat" file) ~doc: "File containing physical egress"
  )
  let run _ _ _ _ _ _ _ _ _ _ _ () = printf "dummy!"
end



(*===========================================================================*)
(* BASIC SPECIFICATION OF COMMANDS                                           *)
(*===========================================================================*)

let local : Command.t =
  Command.basic
    ~summary:"run local compiler"
    (* ~readme: *)
    Local.spec
    Local.run

let global : Command.t =
  Command.basic
    ~summary:"run global compiler"
    (* ~readme: *)
    Global.spec
    Global.run

let virt : Command.t =
  Command.basic
    ~summary:"run virtual compiler"
    (* ~readme: *)
    Virtual.spec
    Virtual.run

let main : Command.t =
  Command.group
    ~summary:"Runs (local/global/virtual) compiler and dumps resulting flow tables"
    (* ~readme: *)
    [("local", local); ("global", global); ("virtual", virt)]
