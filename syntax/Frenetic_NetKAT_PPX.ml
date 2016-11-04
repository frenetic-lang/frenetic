open! StdLabels
open Ppx_core.Std
open Parsetree
open Ast_builder.Default

let ext_name = "nk"

let expand_let_body body =
  let loc = body.pexp_loc in
  let pexp_desc =
    match body.pexp_desc with
    | Pexp_constant (Pconst_string (s, Some "")) ->
      (* FIXME: this is where we would call the NetKAT parser, together with a source location *)
      Pexp_constant (Pconst_string ("NetKAT is awesome!!!", Some ""))
    | _ -> Location.raise_errorf ~loc "'let%%%s' may only bind quoted NetKAT" ext_name
  in
  { body with pexp_desc }

let expand ~loc:_ ~path:_ expr =
  let loc = expr.pexp_loc in
  let pexp_desc =
    match expr.pexp_desc with
    | Pexp_let (Nonrecursive, bindings, body) ->
      Pexp_let (Nonrecursive, bindings, expand_let_body body)
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
    expand

let () =
  (* SJS: deprecated; use rule instead *)
  Ppx_driver.register_transformation "netkat" ~extensions:[nk_ext]
