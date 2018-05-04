open Core
open Frenetic_decision

let decide : Command.t =
  Command.basic_spec
    ~summary:"Invokes decision procedure with provided file."
    Command.Spec.(
      empty
      +> anon ("file" %: file)
    )
    (fun file () ->
      Frenetic_kernel.Util.pp_exceptions ();
      let formula = Parser.formula_of_file file in
      Format.printf "%a\n%!" (Ast.pp_formula String.pp) formula
    )

let () =
  Command.run ~version: "0.1" ~build_info: "RWO" decide
