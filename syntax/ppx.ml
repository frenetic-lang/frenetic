open Ppx_core.Std
open Parsetree

(* TODO: also support let declarations and constant expressions *)

(* extension is triggered by keword 'nk' *)
let ext_keyw = "nk"

let expand_netkat ~loc s =
  (* FIXME: this is where we would call the NetKAT parser, together with a source location *)
    Pexp_constant (Pconst_string ("NetKAT is awesome!!!", Some ""))

let expand_binding binding =
  let loc = binding.pvb_expr.pexp_loc in
  let pexp_desc  =
    match binding.pvb_expr.pexp_desc with
    | Pexp_constant (Pconst_string (s, Some "")) ->
      expand_netkat ~loc s
    | _ ->
      Location.raise_errorf ~loc "'let%%%s' may only bind quoted NetKAT" ext_keyw
  in
  { binding with pvb_expr = { binding.pvb_expr with pexp_desc } }

let expand_expr ~loc:_ ~path:_ expr =
  let loc = expr.pexp_loc in
  let pexp_desc =
    match expr.pexp_desc with
    | Pexp_let (Nonrecursive, bindings, body) ->
      Pexp_let (Nonrecursive, List.map expand_binding bindings, body)
    | Pexp_let (Recursive, _, _) ->
      Location.raise_errorf ~loc "'let%%%s' may not be recursive" ext_keyw
    | _ ->
      Location.raise_errorf ~loc "'%%%s' can only be used with 'let'" ext_keyw
  in
  { expr with pexp_desc }

let expand_struct_item ~loc:_ ~path:_ sitem =
  let loc = sitem.pstr_loc in
  let pstr_desc =
    match sitem.pstr_desc with
    | Pstr_value (Nonrecursive, bindings) ->
      Pstr_value (Nonrecursive, List.map expand_binding bindings)
    | Pstr_value (Recursive, _) ->
      Location.raise_errorf ~loc "'let%%%s' may not be recursive" ext_keyw
    | _ ->
      Location.raise_errorf ~loc "'%%%s' can only be used with 'let'" ext_keyw
  in
  { sitem with pstr_desc }

(* syntax extension for let-expressions *)
let nk_ext_expr =
  Extension.V2.declare
    ext_keyw
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_expr

(* syntax extension for let-declarations *)
let nk_ext_struct =
  Extension.V2.declare
    ext_keyw
    Extension.Context.structure_item
    Ast_pattern.(pstr (__ ^:: nil))
    expand_struct_item

let () =
  (* SJS: deprecated; use rule instead *)
  Ppx_driver.register_transformation "netkat"
    ~extensions:[nk_ext_expr; nk_ext_struct];
  Ppx_driver.standalone ()
