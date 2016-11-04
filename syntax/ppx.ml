open Ppx_core.Std
open Parsetree

(* TODO: also support let declarations and constant expressions *)

let ext_name = "nk"

let expand_binding binding =
  let loc = binding.pvb_expr.pexp_loc in
  let pexp_desc  =
    match binding.pvb_expr.pexp_desc with
    | Pexp_constant (Pconst_string (s, Some "")) ->
      (* FIXME: this is where we would call the NetKAT parser, together with a source location *)
      Pexp_constant (Pconst_string ("NetKAT is awesome!!!", Some ""))
    | _ -> Location.raise_errorf ~loc "'let%%%s' may only bind quoted NetKAT" ext_name
  in
  { binding with pvb_expr = { binding.pvb_expr with pexp_desc } }

let expand_expr ~loc:_ ~path:_ expr =
  let loc = expr.pexp_loc in
  let pexp_desc =
    match expr.pexp_desc with
    | Pexp_let (Nonrecursive, bindings, body) ->
      Pexp_let (Nonrecursive, List.map expand_binding bindings, body)
    | Pexp_let (Recursive, _, _) ->
      Location.raise_errorf ~loc "'let%%%s' may not be recursive" ext_name
    | _ ->
      Location.raise_errorf ~loc "'%%%s' can only be used with 'let'" ext_name
  in
  { expr with pexp_desc }

let nk_ext =
  Extension.V2.declare
    "nk"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_expr

let () =
  (* SJS: deprecated; use rule instead *)
  Ppx_driver.register_transformation "netkat" ~extensions:[nk_ext];
  Ppx_driver.standalone ()
