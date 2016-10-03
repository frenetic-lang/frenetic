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

;; NetKAT basics, based on, but different from the netkat.z3 file
(set-option :smt.mbqi true)
;; Values
(define-sort Value () (_ BitVec 32))

;; Fields
(declare-datatypes ()
  ((Field
      Switch
      Location
      EthType
      EthSrc
      EthDst
      Vlan
      VlanPcp
      IPProto
      IP4Src
      IP4Dst
      TCPSrcPort
      TCPDstPort
      VPort
      VSwitch
      VFabric
      Channel
      Meta
      )))

;; Predicates. Different from netkat.z3 where this is a function, not a
;; type. This is not used in the rest of this file because there is no way to
;; define recursive procedures or pattern matching, and Z3 does not produce
;; inductive proofs.
(declare-datatypes (F V) ((Predicate
                           Tru
                           Fls
                           (Test (field F) (value V))
                           (And (a1 Predicate) (a2 Predicate))
                           (Or  (o1 Predicate) (o2 Predicate))
                           (Neg (n Predicate)))))

;; Datatype for locations, pairs of switch and port
(declare-datatypes () ((Location (mk-loc (switch Value) (port Value)))))


;; Encode a topology as a map from locations to an array of (connected) locations
(define-sort Locations () (Array Int Location))
(define-sort Topology  () (Array Location Location))

;; A single conditional is a triple of (bool, field, value) where the boolean is
;; true for a positive match, and false for a negative match. The condition for
;; a dyad is int-indexed array of single conditionals.
(declare-datatypes () ((Cond
                        (mk-cond (match Bool) (field Field) (value Value)))))

(define-sort Condition () ( Array Int Cond))

;; Actions are arrays with fields as indices and values as the domain. Since
;; fields can only be assigned to once, this is fine.
(define-sort Action () (Array Field Value))

;; Datatype for dyads, parameterized on locations (L), conditions (C) and
;; actions (A)
(declare-datatypes (L C A) ((Dyad
                             (mk-dyad (src L) (dst L) (condition C) (action A)))))

;; Check if a condition is a check on locations only
(declare-const c1 Condition)
(assert (= c1 (store c1 0 (mk-cond true Switch #x00000001))))
(assert (= c1 (store c1 1 (mk-cond true Location #x00000002))))

(declare-const c2 Condition)
(assert (= c2 (store c2 0 (mk-cond true Switch #x00000001))))
(assert (= c2 (store c2 1 (mk-cond true EthType #x00000002))))

(declare-const length Int)
(assert (= length 2))                   ; Where `2` is the length of the condition
(define-fun places-only ((c Condition) (length Int)) Bool
  (forall ((index Int))
          (implies
           (and (< 0 index)
                (< index length))
           (let ((fld (field (select c index) )))
             (or (= fld Switch) (= fld Location))))))

(assert (places-only c1 length))
(assert (not (places-only c2 length)))

;; Check if one condition (c1) is a subset of another (c2)
(declare-const c3 Condition)
(declare-const l3 Int)
(assert (= l3 2))
(assert (= c3 (store c3 0 (mk-cond true Switch #x00000001))))
(assert (= c3 (store c3 1 (mk-cond true Location #x00000002))))

(declare-const c4 Condition)
(declare-const l4 Int)
(assert (= l4 3))
(assert (= c4 (store c4 0 (mk-cond true Switch #x00000001))))
(assert (= c4 (store c4 1 (mk-cond true Location #x00000002))))
(assert (= c4 (store c4 2 (mk-cond true EthType #x00000002))))

(define-fun subset-of ((c1 Condition) (l1 Int) (c2 Condition) (l2 Int)) Bool
  (and (<= l1 l2)
       (forall ((i1 Int))
               (implies
                (and (< 0 i1) (< i1 l1))
                (exists ((i2 Int))
                   (implies (and (< 0 i2) (< l2 l2))
                            (= (field (select c1 i1)) (field (select c2 i2)))))))))

(assert ( subset-of c3 l3 c4 l4 ))

;; Define the adjacency function on the topology representation
(declare-const t1 Topology)
;; s1:p2 -- s2:p1
;; s1:p3 -- s3:p2
(assert (= t1 (store t1 (mk-loc #x00000001 #x00000002)
                        (mk-loc #x00000002 #x00000001))))
(assert (= t1 (store t1 (mk-loc #x00000002 #x00000001)
                        (mk-loc #x00000001 #x00000002))))
(assert (= t1 (store t1 (mk-loc #x00000001 #x00000003)
                        (mk-loc #x00000003 #x00000001))))
(assert (= t1 (store t1 (mk-loc #x00000003 #x00000001)
                        (mk-loc #x00000001 #x00000003))))


(define-fun adjacent ((t Topology) (l1 Location) (l2 Location)) Bool
  (or (= (switch l1) (switch l2))
      (exists ((p1 Value) (p2 Value))
         (let ((l11 (select t l2))
               (l21 (select t l1)))
           (or (and (= p1 (port l11))
                    (= (switch l1) (switch l11)))
               (and (= p2 (port l21))
                    (= (switch l2) (switch l21))))))))

(assert (adjacent t1 (mk-loc #x00000001 #x00000002) (mk-loc #x00000002 #x00000001)))
(assert (adjacent t1 (mk-loc #x00000003 #x00000001) (mk-loc #x00000001 #x00000003)))

(assert (adjacent t1 (mk-loc #x00000003 #x00000001) (mk-loc #x00000001 #x00000002)))
(assert (not (adjacent t1 (mk-loc #x00000003 #x00000002) (mk-loc #x00000002 #x00000003))))


(check-sat)
(get-model)
