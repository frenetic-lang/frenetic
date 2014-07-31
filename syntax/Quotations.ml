open Camlp4.PreCast
open MyLexer
module Q = Syntax.Quotation

let nk_eoi = Parser.Gram.Entry.mk "nk_eoi"
let nkpred_eoi = Parser.Gram.Entry.mk "nkpred_eoi"

EXTEND Parser.Gram
  nk_eoi: [[ x = Parser.nk_pol; `MyLexer.EOI -> x ]];
  nkpred_eoi: [[ x = Parser.nk_pred; `MyLexer.EOI -> x ]];
END

let parse_netkat loc _ s =
  let q = !Camlp4_config.antiquotations in
  Camlp4_config.antiquotations := true;
  let result = Parser.Gram.parse_string nk_eoi loc s in
  Camlp4_config.antiquotations := q;
  result

let parse_netkat_pred loc _ s =
  let q = !Camlp4_config.antiquotations in
  Camlp4_config.antiquotations := true;
  let result = Parser.Gram.parse_string nkpred_eoi loc s in
  Camlp4_config.antiquotations := q;
  result

let () =
  Q.add "netkat" Q.DynAst.expr_tag parse_netkat;
  Q.add "nkpred" Q.DynAst.expr_tag parse_netkat_pred;
  Q.default := "netkat";
