(* type graph = (switchId * switchId * int) list *)

type regex =
    Hop of OpenFlow0x01Types.switchId
  | Host of int
  | Star
  | Sequence of regex * regex
  | Union of regex * regex
  | Intersection of regex * regex

type regex_policy =
    RegPol of NetCoreFT.predicate * regex * int
  | RegUnion of regex_policy * regex_policy
  | RegInter of regex_policy * regex_policy

val ( <+> ) : regex_policy -> regex_policy -> regex_policy
val ( <*> ) : regex_policy -> regex_policy -> regex_policy
val ( && ) : regex -> regex -> regex
val ( || ) : regex -> regex -> regex
val ( <.> ) : regex -> regex -> regex

val flatten_reg : regex -> regex list

val install_hosts : regex list -> Graph.Graph.graph -> (regex * regex) list

val expand_regex_with_match :
  regex list -> Graph.Graph.a -> Graph.Graph.graph -> (regex * regex) list

val expand_path_with_match :
  regex list -> Graph.Graph.graph -> (regex * regex) list

val compile_regex : regex_policy -> Graph.Graph.graph -> NetCoreFT.policy
