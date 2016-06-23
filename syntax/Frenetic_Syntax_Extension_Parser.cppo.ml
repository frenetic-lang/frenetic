#define AST
#include "Frenetic_NetKAT_Parser.cppo.ml"

let parse_netkat_pol loc _ s =
  let q = !Camlp4_config.antiquotations in
  Camlp4_config.antiquotations := true;
  let result = policy_of_string ~loc s in
  Camlp4_config.antiquotations := q;
  result

let parse_netkat_pred loc _ s =
  let q = !Camlp4_config.antiquotations in
  Camlp4_config.antiquotations := true;
  let result = pred_of_string ~loc s in
  Camlp4_config.antiquotations := q;
  result

let () =
  let module Q = Syntax.Quotation in
  Q.add "netkat" Q.DynAst.expr_tag parse_netkat_pol;
  Q.add "nkpred" Q.DynAst.expr_tag parse_netkat_pred;
  Q.default := "netkat";
