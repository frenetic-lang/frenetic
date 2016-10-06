;; Basic set definition
(define-sort Set (T) (Array T Bool))

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
(declare-datatypes () ((Location disconnected (mk-loc (switch Value) (port Value)))))


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
(declare-datatypes () ((Mod
                        preserve
                        (modify (value Value)))))
(define-sort Action () (Array Field Mod))

;; Datatype for dyads, parameterized on locations (L), conditions (C) and
;; actions (A)
(declare-datatypes (L C A) ((Dyad
                             (mk-dyad (src L) (dst L) (condition C) (action A)))))

;; Decider primitives

;; Primitives on conditions
(define-fun tests ((c Condition) (length Int) (f Field)) Bool
  (exists ((index Int))
     (implies
      (and (< 0 index)
           (< index length))
      (= f (field (select c index))))))

(define-fun tests-only ((c Condition) (clen Int) (f (Set Field))) Bool
  (forall ((cin Int))
          (implies
           (and (< 0 cin)
                (< cin clen))
           (exists ((fld Field))
              (and (= fld (field (select c cin)))
                   (select f fld))))))

(define-fun tests-all ((c Condition) (clen Int) (f (Set Field))) Bool
  (forall ((fld Field))
          (implies
           (select f fld)
           (exists ((cin Int))
              (and (< 0 cin)
                   (< cin clen)
                   (= fld (field (select c cin))))))))

(define-fun tests-exactly ((c Condition) (clen Int) (f (Set Field))) Bool
  (and (tests-only c clen f)
       (tests-all  c clen f)))

;; Primitives on actions
;; preserves and modifies are negations of each other
;; Defining preserves-only or modifies-all, and the -exact versions seems
;; unnecessary, but they are fairly trivial, so they are included for completeness

(define-fun preserves ((a Action) (f Field)) Bool
  (= preserve (select a f)))

(define-fun modifies ((a Action) (f Field)) Bool
  (not (preserves a f)))

(define-fun preserves-all ((a Action) (f (Set Field))) Bool
  (forall ((fld Field))
          (implies
           (select f fld)
           (preserves a fld))))

(define-fun preserves-only ((a Action) (f (Set Field))) Bool
  (forall ((fld Field))
          (implies
           (preserves a fld)
           (select    f fld))))

(define-fun preserves-exactly ((a Action) (f (Set Field))) Bool
  (and (preserves-only a f)
       (preserves-all  a f)))

(define-fun modifies-all ((a Action) (f (Set Field))) Bool
  (forall ((fld Field))
          (implies
           (select f fld)
           (modifies a fld))))

(define-fun modifies-only ((a Action) (f (Set Field))) Bool
  (forall ((fld Field))
          (implies
           (modifies a fld)
           (select   f fld))))

(define-fun modifies-exactly ((a Action) (f (Set Field))) Bool
   (and (modifies-only a f)
        (modifies-all  a f)))


;; Decider functions
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
  (ite (or (= l1 disconnected) (= l2 disconnected))
       false
  (or (= (switch l1) (switch l2))
      (exists ((l11 Location))
         (and (= l11 (select t l2))
              (not (= l11 disconnected))
              (= (switch l1) (switch l11))))
      (exists ((l21 Location))
         (and (= l21 (select t l1))
              (not (= l21 disconnected))
              (= (switch l2) (switch l21)))))))

