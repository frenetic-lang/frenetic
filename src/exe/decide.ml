open Core
open Frenetic_decision

module Flag = struct
  open Command.Spec

  let stdin =
    flag "--stdin" no_arg ~doc:"Read from stdin instead of from file."
end


let spec = Command.Spec.(
  empty
  +> Flag.stdin
  +> anon ("file-or-formula" %: file)
)

let run stdin input () =
  Frenetic_kernel.Util.pp_exceptions ();
  let parse =
    if stdin then Parser.formula_of_string ?pos:None
    else Parser.formula_of_file
  in
  let formula = parse input in
  Format.printf "%a\n%!" (Ast.pp_formula String.pp) formula

let cmd : Command.t =
  Command.basic_spec
    ~summary:"Invokes decision procedure with provided file."
    spec
    run

let () = Command.run ~version: "0.1" ~build_info: "RWO" cmd
