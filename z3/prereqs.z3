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

;; Decider functions

;; Places only
(define-fun places-only ((c Condition) (length Int)) Bool
  (forall ((index Int))
          (implies
           (and (< 0 index)
                (< index length))
           (let ((fld (field (select c index) )))
             (or (= fld Switch) (= fld Location))))))


(define-fun subset-of ((c1 Condition) (l1 Int) (c2 Condition) (l2 Int)) Bool
  (and (<= l1 l2)
       (forall ((i1 Int))
               (implies
                (and (< 0 i1) (< i1 l1))
                (exists ((i2 Int))
                   (implies (and (< 0 i2) (< l2 l2))
                            (= (field (select c1 i1)) (field (select c2 i2)))))))))


(define-fun adjacent ((t Topology) (l1 Location) (l2 Location)) Bool
  (or (= (switch l1) (switch l2))
      (exists ((l11 Location))
         (and (= l11 (select t l2))
              (= (switch l1) (switch l11))))
      (exists ((l21 Location))
         (and (= l21 (select t l1))
              (= (switch l2) (switch l21))))))

