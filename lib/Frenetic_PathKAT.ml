open Core.Std
open Frenetic_NetKAT
open Frenetic_Fabric

type path = pred * loc list

module Parser = struct

  (** Monadic Parsers for the command line *)
  open MParser
  module Tokens = MParser_RE.Tokens

  let symbol = Tokens.symbol

  let pred : (pred, bytes list) MParser.t =
    char '[' >>
    many_chars_until (none_of "]") (char ']') >>= fun s ->
    return (Frenetic_NetKAT_Parser.pred_of_string s)

  let location : (loc, bytes list) MParser.t =
    many_chars_until digit (char '@') >>= fun swid ->
    many_chars digit >>= fun ptid ->
    return ((Int64.of_string swid),
            (Int32.of_string ptid))

  let path : (path, bytes list) MParser.t =
    pred >>= fun p ->
    spaces >> (char ':') >> spaces >>
    location >>= fun start ->
    (many_until (spaces >> symbol "==>" >> spaces >> location >>= fun l ->
                 return l)
       (char ';')) >>= fun ls ->
    return (p, start::ls)

  let program : (path list, bytes list) MParser.t =
    many_until (spaces >> path) eof

end

let paths_of_string (s:string) : (path list, string) Result.t =
  match (MParser.parse_string Parser.program s []) with
  | Success paths -> Ok paths
  | Failed (msg, e) -> Error msg
