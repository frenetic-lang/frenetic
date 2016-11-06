open Ppx_core.Std
open Parsetree

(* extension is triggered by keword 'nk' *)
let ext_keyw = "nk"

(* expands `s` in `let%nk x = {| s |}` *)
let expand_nk_string ~loc s =
  (* FIXME: this is where we would call the NetKAT parser, together with a source location *)
    Pexp_constant (Pconst_string ("NetKAT is awesome!!!", None))

(* expands `e` in `let%nk x = e` *)
let expand_bound_expr expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  (* only expand e if e = {| s |} *)
  | Pexp_constant (Pconst_string (s, Some "")) ->
    { expr with pexp_desc = expand_nk_string ~loc s }
  | _ ->
    Location.raise_errorf ~loc "'let%%%s' may only bind quoted NetKAT" ext_keyw

(* expands `x=e` in `let%nk x = e` *)
let expand_binding binding =
  { binding with pvb_expr = expand_bound_expr binding.pvb_expr }

(* expands `let%nk <bindings>` *)
let expand_let_decl ~loc ~path:_ bindings =
  let module B = Ast_builder.Make(struct let loc = loc end) in
  B.(pstr_value Nonrecursive (List.map expand_binding bindings))

(* expands `let%nk <bindings> in body` *)
let expand_let_expr ~loc ~path:_ bindings body =
  let module B = Ast_builder.Make(struct let loc = loc end) in
  B.(pexp_let Nonrecursive (List.map expand_binding bindings) body)

(* declare `let%nk x = e` extension *)
let nk_ext_struct =
  Extension.V2.declare
    ext_keyw
    Extension.Context.structure_item
    Ast_pattern.(pstr (pstr_value nonrecursive __ ^:: nil))
    expand_let_decl

(* declare `let%nk x = e in b` extension *)
let nk_ext_expr =
  Extension.V2.declare
    ext_keyw
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (pexp_let nonrecursive __ __))
    expand_let_expr

let () =
  Ppx_driver.register_transformation "netkat"
    ~extensions:[nk_ext_expr; nk_ext_struct];
  (* FIXME: just for testing. Replace with Ppx_driver.run_as_ppx_rewriter *)
  Ppx_driver.standalone ()
