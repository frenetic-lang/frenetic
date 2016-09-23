;; This is an attempt to encode the decider functions we need for edge program
;; synthesis as SMT formulas.

;; We call alpha/beta pairs "dyads". Merriam Webster lists dyad as a synonym for
;; "pair": http://www.merriam-webster.com/thesaurus/pair.
;; The OCaml type of a dyad is a quadruple of (source, destination, condition,
;; action).
;; The source and destination are presented separately for convenience only and
;; are derived from the condition when a dyad is produced from a FDD.

;; Given two dyads, d1 (from a policy) and d2 (from a fabric), we wish to decide
;; if d2 can be used to implement d1 in the fabric. Additionally, we might
;; require a topology to provide information such as adjacency between nodes.

;; Example 1: For an optical fabric, we only require the policy dyad's endpoints
;; to be adjacent to the fabric dyad's endpoints, and for the fabric dyad to be
;; conditioned only on locations, and not on other packet headers.

;; We can express this as an OCaml function, given the appropriate helper functions:

;; let decide topo ((_,_,cond,_) as pol) ((_,_,cond',_) as fab) =
;;   adjacent topo pol fab && Fabric.Condition.places_only cond'


;; Example 2:
;; Given a simple L2/L3 fabric, without any encapsulation capability on the edge switches,
;; a fabric dyad can carry a policy dyad, iff:
;;   1. The endpoints of the two dyads are adjacent (see the `adjacent` function)
;;   and
;;   2. The fabric dyad's conditions only require incoming traffic to enter at
;;    a certain location only or
;;   3. The set of fields checked by the fabric dyad's conditions are a subset
;;    of those checked by the policy dyad.

;; Again, this can be expressed as an OCaml function, given appropriate helpers:

;; let decide topo
;;     ((src,dst,cond,actions) as pol)
;;     ((src',dst',cond',actions') as fab) =
;;   adjacent topo pol fab &&
;;   ( Fabric.Condition.places_only cond' || Fabric.Condition.is_subset cond' cond)

