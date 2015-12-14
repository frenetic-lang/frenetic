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
  let spec = Command.Spec.empty
  let run () = printf "dummy!"
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
    ~summary:"Runs compiler and dumps resulting flow tables"
    (* ~readme: *)
    [("local", local); ("global", global); ("virtual", virt)]

let () = Command.run ~version:"1.0" main
