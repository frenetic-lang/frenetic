;; Fields
(declare-datatypes () 
  ((Field Switch Port 
      EthType 
      EthSrc 
      EthDst 
      Vlan 
      VlanPcp 
      IPProto 
      IP4Src 
      IP4Dst 
      TCPSrcPort 
      TCPDstPort)))

;; Predicates
(declare-datatypes () 
  ((Predicate 
      PrTrue 
      PrFalse 
      (PrTest (prTestField Field) (prTestValue Int)) 
      (PrAnd (prAnd1 Predicate) (prAnd2 Predicate)) 
      (PrOr (prOr1 Predicate) (prOr2 Predicate)) 
      (PrNot (prNot Predicate)))))

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

;; NetKAT Natural Order
(declare-rel Leq (Policy Policy)) 
(assert 
  (forall ((p Policy) (q Policy))
    (iff (Leq p q) (Eq (PoUnion p q) q))))

;; Reflexive
(assert 
  (forall ((p Policy))
    (Eq p p)))

;; Symmetric
(assert 
  (forall ((p Policy) (q Policy))
    (implies (Eq p q) (Eq q p))))

;; Transitive
(assert 
  (forall ((p Policy) (q Policy) (r Policy))
    (implies (and (Eq p q) (Eq q r)) (Eq p r))))

;; Congruence PoUnion
(assert 
  (forall ((p Policy) (q Policy) (r Policy))
    (implies (Eq p q) (Eq (PoUnion p r) (PoUnion q r)))))

;; Congruence PoSeq
(assert 
  (forall ((p Policy) (q Policy) (r Policy))
    (implies (Eq p q) (Eq (PoSeq p r) (PoSeq q r)))))

;; Congruence PoStar
(assert 
  (forall ((p Policy) (q Policy))
    (implies (Eq p q) (Eq (PoStar p) (PoStar q)))))

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
(assert 
  (forall ((p Policy) (q Policy) (r Policy)) 
    (implies (Leq (PoUnion q (PoSeq p r)) r)
             (Leq (PoSeq (PoStar p) q) r))))

;; KA-Unroll-R
(assert 
  (forall ((p Policy))
    (Eq (PoStar p) (PoUnion (PoFilter PrTrue) (PoSeq (PoStar p) p)))))

;; KA-LFP-R
(assert 
  (forall ((p Policy) (q Policy) (r Policy)) 
    (implies (Leq (PoUnion p (PoSeq q r)) q)
             (Leq (PoSeq p (PoStar r)) q))))

(check-sat)
