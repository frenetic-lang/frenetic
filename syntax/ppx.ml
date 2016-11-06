open Ppx_core.Std
open Parsetree

(* extension is triggered by keword 'nk' *)
let ext_keyw = "nk"

let expand_netkat ~loc s =
  (* FIXME: this is where we would call the NetKAT parser, together with a source location *)
    Pexp_constant (Pconst_string ("NetKAT is awesome!!!", None))

let expand_binding_expr expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (s, Some "")) ->
    { expr with pexp_desc = expand_netkat ~loc s }
  | _ ->
    Location.raise_errorf ~loc "'let%%%s' may only bind quoted NetKAT" ext_keyw

let expand_binding binding =
  { binding with pvb_expr = expand_binding_expr binding.pvb_expr }

let expand_let_decl ~loc ~path:_ bindings =
  let module B = Ast_builder.Make(struct let loc = loc end) in
  B.(pstr_value Nonrecursive (List.map expand_binding bindings))

let expand_let_expr ~loc ~path:_ bindings body =
  let module B = Ast_builder.Make(struct let loc = loc end) in
  B.(pexp_let Nonrecursive (List.map expand_binding bindings) body)

let nk_ext_struct =
  Extension.V2.declare
    ext_keyw
    Extension.Context.structure_item
    Ast_pattern.(pstr (pstr_value nonrecursive __ ^:: nil))
    expand_let_decl

let nk_ext_expr =
  Extension.V2.declare
    ext_keyw
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (pexp_let nonrecursive __ __))
    expand_let_expr

let () =
  (* SJS: deprecated; use rule instead *)
  Ppx_driver.register_transformation "netkat"
    ~extensions:[nk_ext_expr; nk_ext_struct];
  Ppx_driver.standalone ()
