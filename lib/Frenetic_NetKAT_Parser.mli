(* Simple Camlp4 parser for NetKAT *)

(** Same as policy_of_string, but NetKAT policy read from character stream *)
val policy_of_stream : ?loc:Camlp4.PreCast.Loc.t -> char Stream.t -> Frenetic_NetKAT.policy

(** Same as pred_of_string, but NetKAT predicate read from character stream *)
val pred_of_stream : ?loc:Camlp4.PreCast.Loc.t -> char Stream.t -> Frenetic_NetKAT.pred

(** Given a policy string, parse it into abstract NetKAT form *)
val policy_of_string : ?loc:Camlp4.PreCast.Loc.t -> string -> Frenetic_NetKAT.policy

(** Given a predicate string, parse it into abstract NetKAT form.  Raise exception if the string is a policy, not a predicate *)
val pred_of_string : ?loc:Camlp4.PreCast.Loc.t -> string -> Frenetic_NetKAT.pred

(** Same as policy_of_string, but NetKAT policy read from file *)
val policy_of_file : string -> Frenetic_NetKAT.policy

(** Same as pred_of_string, but NetKAT predicate read from file *)
val pred_of_file : string -> Frenetic_NetKAT.pred
