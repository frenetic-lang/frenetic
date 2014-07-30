open Camlp4.PreCast
open MyLexer
module Q = Syntax.Quotation

let nk_eoi = Parser.Gram.Entry.mk "nk_eoi"

EXTEND Parser.Gram
  nk_eoi: [ [ x = Parser.nk_pol; `MyLexer.EOI -> x ] ];
END

let parse_netkat loc _ s =
  let q = !Camlp4_config.antiquotations in
  Camlp4_config.antiquotations := true;
  let result = Parser.Gram.parse_string nk_eoi loc s in
  Camlp4_config.antiquotations := q;
  result

let () =
  Q.add "netkat" Q.DynAst.expr_tag parse_netkat;
  Q.default := "netkat";
