;; Packets
(declare-datatypes () 
  ((Packet 
     (Packet (src Int) (dst Int)))))

;; Fields
(declare-datatypes () 
  ((Field Src Dst)))

;; Predicates
(declare-datatypes () 
  ((Predicate 
      PrTrue 
      PrFalse 
      (PrTest (prTestField Field) (prTestValue Int)) 
      (PrAnd (prAnd1 Predicate) (prAnd2 Predicate)) 
      (PrOr (prAnd1 Predicate) (prAnd2 Predicate)) 
      (PrNot (pr1 Predicate)))))

;; Policies
(declare-datatypes () 
  ((Policy 
     (PoFilter (pred Predicate)) 
     (PoModify (poModifyField Field) (poModifyValue Int)) 
     (PoUnion (poUnion1 Policy) (poUnion2 Policy)) 
     (PoSeq (poSeq1 Policy) (poSeq2 Policy)) 
     (PoStar (poStar Policy)) PoDup)))

;; NetKAT Equivalence
(declare-rel Eq (Policy Policy)) 

;; KA-Plus-Assoc
(assert 
  (forall ((p Policy) (q Policy) (r Policy)) 
    (Eq (PoUnion (PoUnion p q) r) (PoUnion p (PoUnion q r)))))

;; KA-Plus-Comm
(assert 
  (forall ((p Policy) (q Policy))
    (Eq (PoUnion p q) (PoUnion q p))))

;; KA-Plus-Zero
(assert 
  (forall ((p Policy))
    (Eq (PoUnion p (PoFilter PrFalse)) p)))

;; KA-Plus-Idem
(assert 
  (forall ((p Policy))
    (Eq (PoUnion p p) p)))

;; KA-Seq-Assoc
(assert 
  (forall ((p Policy) (q Policy) (r Policy)) 
    (Eq (PoSeq (PoSeq p q) r) (PoSeq p (PoSeq q r)))))

;; KA-Seq-One
(assert 
  (forall ((p Policy))
    (Eq (PoSeq p (PoFilter PrTrue)) p)))

;; KA-One-Seq
(assert 
  (forall ((p Policy))
    (Eq (PoSeq (PoFilter PrTrue) p) p)))

;; KA-Seq-Dist-L
(assert 
  (forall ((p Policy) (q Policy) (r Policy)) 
     (Eq (PoSeq p (PoUnion q r)) (PoUnion (PoSeq p q) (PoSeq p r)))))

;; KA-Seq-Dist-R
(assert 
  (forall ((p Policy) (q Policy) (r Policy)) 
     (Eq (PoSeq (PoUnion p q) r) (PoUnion (PoSeq p r) (PoSeq q r)))))

;; KA-Seq-Zero
(assert 
  (forall ((p Policy))
    (Eq (PoSeq p (PoFilter PrFalse)) (PoFilter PrFalse))))

;; KA-Zero-Seq
(assert 
  (forall ((p Policy))
    (Eq (PoSeq (PoFilter PrFalse) p) (PoFilter PrFalse))))

;; KA-Unroll-L
(assert 
  (forall ((p Policy))
    (Eq (PoStar p) (PoUnion (PoFilter PrTrue) (PoSeq p (PoStar p))))))

;; KA-LFP-L
;; (assert 
;;   (forall ((p Policy) (q Policy) (r Policy)) 
;;     (implies (Eq (PoUnion (PoUnion q (PoSeq p r)) r) r)
;;              (Eq (PoUnion (PoSeq (PoStar p) q) r) r))))

;; KA-Unroll-R
(assert 
  (forall ((p Policy))
    (Eq (PoStar p) (PoUnion (PoFilter PrTrue) (PoSeq (PoStar p) p)))))

(check-sat)
(get-model)
