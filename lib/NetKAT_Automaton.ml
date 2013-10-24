type policy = NetKAT_Types.policy
type pred = NetKAT_Types.pred
type header = NetKAT_Types.header
type header_val = NetKAT_Types.header_val

type lf_policy =
  | Filter of pred
  | Mod of header * header_val
  | Par of lf_policy * lf_policy
  | Choice of lf_policy * lf_policy
  | Seq of lf_policy * lf_policy
  | Star of lf_policy

type link = header_val * header_val * header_val * header_val

type regex =
  | Char of lf_policy * link
  | Alt of regex * regex
  | Cat of regex * regex
  | Kleene of regex
  | Empty

let regex_of_policy (p : policy) : regex =
  failwith "nyi"

(*
 * Helper function for `regex_of_policy`. Straight up replace constructors from
 * this module with the identically-named constructors from `NetKAT_Types`.
 *)
let rec lf_policy_to_policy (lfp : lf_policy) : policy =
  match lfp with
    | Filter(p) -> NetKAT_Types.Filter(p)
    | Mod(h, v) -> NetKAT_Types.Mod(h, v)
    | Par(p1, p2) ->
      NetKAT_Types.Par(lf_policy_to_policy p1, lf_policy_to_policy p2)
    | Choice(p1, p2) ->
      NetKAT_Types.Choice(lf_policy_to_policy p1, lf_policy_to_policy p2)
    | Seq(p1, p2) ->
      NetKAT_Types.Seq(lf_policy_to_policy p1, lf_policy_to_policy p2)
    | Star(p) ->
      NetKAT_Types.Star(lf_policy_to_policy p)

(* Helper function for `regex_of_policy`. Take care of link translation *)
let link_to_policy ((sw1, pt1, sw2, pt2) : link) : policy =
  NetKAT_Types.Link(sw1, pt1, sw2, pt2)

let rec regex_to_policy (r : regex) : policy =
  match r with
    | Char(lfp, l) ->
      NetKAT_Types.Seq(lf_policy_to_policy lfp, link_to_policy l)
    | Alt(r1, r2) ->
      NetKAT_Types.Choice(regex_to_policy r1, regex_to_policy r2)
    | Cat(r1, r2) ->
      NetKAT_Types.Seq(regex_to_policy r1, regex_to_policy r2)
    | Kleene(r) ->
      NetKAT_Types.Star(regex_to_policy r)
    | Empty -> NetKAT_Types.Filter(NetKAT_Types.True)
