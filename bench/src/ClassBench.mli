val parse_classbench : filename:string -> Netkat.Syntax.pred
(* Parse a classbench-generated ACL into a NetKAT predicate. Assumes all
   entries are "allow" and everything else is "deny". *)
