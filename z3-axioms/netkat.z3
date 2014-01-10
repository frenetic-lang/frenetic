;; Values
(define-sort Value () Bool)

;; Fields
(declare-datatypes () 
  ((Field 
      Switch 
      Port 
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
      )))

;; Predicates
(declare-sort Predicate)
(declare-fun Tru () Predicate)
(declare-fun Fls () Predicate)
(declare-fun Test (Field Value) Predicate)
(declare-fun And (Predicate Predicate) Predicate)
(declare-fun Or (Predicate Predicate) Predicate)
(declare-fun Neg (Predicate) Predicate)

;; Policies
(declare-sort Policy)
(declare-fun Filter (Predicate) Policy)
(declare-fun Modify (Field Value) Policy)
(declare-fun Union (Policy Policy) Policy)
(declare-fun Sequence (Policy Policy) Policy)
(declare-fun Star (Policy) Policy)
;; (declare-fun Dup () Policy)

(declare-rel Leq (Policy Policy))
(assert
  (forall ((p Policy) (q Policy))
     (iff (Leq p q) (= (Union p q) q))))

;;;;;;;;;;;;;;;;;;;;;
;; Boolean Algebra ;;
;;;;;;;;;;;;;;;;;;;;;

;; BA-Non_trivial
;; (assert (not (= Tru Fls)))

;; BA-Plus-Assoc
(assert 
  (forall ((a Predicate) (b Predicate) (c Predicate)) 
    (= (Or (Or a b) c) (Or a (Or b c)))))

;; BA-Plus-Comm
(assert 
  (forall ((a Predicate) (b Predicate))
    (= (Or a b) (Or b a))))

;; BA-Plus-Zero
(assert 
  (forall ((a Predicate))
    (= (Or a Fls) a)))

;; BA-Plus-Idem
(assert 
  (forall ((a Predicate))
    (= (Or a a) a)))

;; BA-Seq-Assoc
(assert 
  (forall ((a Predicate) (b Predicate) (c Predicate)) 
    (= (And (And a b) c) (And a (And b c)))))

;; BA-Seq-One
(assert 
  (forall ((a Predicate))
    (= (And a Tru) a)))

;; BA-One-Seq
(assert 
  (forall ((a Predicate))
    (= (And Tru a) a)))

;; BA-Seq-Dist-L
(assert 
  (forall ((a Predicate) (b Predicate) (c Predicate)) 
     (= (And a (Or b c)) (Or (And a b) (And a c)))))

;; BA-Seq-Dist-R
(assert 
  (forall ((a Predicate) (b Predicate) (c Predicate)) 
     (= (And (Or a b) c) (Or (And a c) (And b c)))))

;; BA-Seq-Zero
(assert 
  (forall ((a Predicate))
    (= (And a Fls) Fls)))

;; BA-Zero-Seq
(assert 
  (forall ((a Predicate))
    (= (And Fls a) Fls)))

;; BA-Plus-Dist
(assert 
  (forall ((a Predicate) (b Predicate) (c Predicate)) 
     (= (Or a (And b c)) (And (Or a b) (Or a c)))))

;; BA-Plus-One
(assert 
  (forall ((a Predicate))
    (= (Or a Tru) Tru)))

;; BA-Excl-Mid
(assert 
  (forall ((a Predicate))
    (= (Or a (Neg a)) Tru)))

;; BA-Seq-Comm
(assert 
  (forall ((a Predicate) (b Predicate))
    (= (And a b) (And b a))))

;; BA-Contra
(assert 
  (forall ((a Predicate))
    (= (And a (Neg a)) Fls)))

;; BA-Seq-Idem
(assert 
  (forall ((a Predicate))
    (= (And a a) a)))

;;;;;;;;;;;;;;;;;;;;
;; Kleene Algebra ;;
;;;;;;;;;;;;;;;;;;;;

;; KA-Filter-Injective
(assert 
  (forall ((a Predicate) (b Predicate))
     (implies (= (Filter a) (Filter b)) (= a b))))

;; KA-Plus-Assoc
(assert 
  (forall ((p Policy) (q Policy) (r Policy)) 
    (= (Union (Union p q) r) (Union p (Union q r)))))

;; KA-Plus-Comm
(assert 
  (forall ((p Policy) (q Policy))
    (= (Union p q) (Union q p))))

;; KA-Plus-Zero
(assert 
  (forall ((p Policy))
    (= (Union p (Filter Fls)) p)))

;; KA-Plus-Idem
(assert 
  (forall ((p Policy))
    (= (Union p p) p)))

;; KA-Seq-Assoc
(assert 
  (forall ((p Policy) (q Policy) (r Policy)) 
    (= (Sequence (Sequence p q) r) (Sequence p (Sequence q r)))))

;; KA-Seq-One
(assert 
  (forall ((p Policy))
    (= (Sequence p (Filter Tru)) p)))

;; KA-One-Seq
(assert 
  (forall ((p Policy))
    (= (Sequence (Filter Tru) p) p)))

;; KA-Seq-Dist-L
(assert 
  (forall ((p Policy) (q Policy) (r Policy)) 
     (= (Sequence p (Union q r)) (Union (Sequence p q) (Sequence p r)))))

;; KA-Seq-Dist-R
(assert 
  (forall ((p Policy) (q Policy) (r Policy)) 
     (= (Sequence (Union p q) r) (Union (Sequence p r) (Sequence q r)))))

;; KA-Seq-Zero
(assert 
  (forall ((p Policy))
    (= (Sequence p (Filter Fls)) (Filter Fls))))

;; KA-Zero-Seq
(assert 
  (forall ((p Policy))
    (= (Sequence (Filter Fls) p) (Filter Fls))))

;; KA-Unroll-L
(assert 
  (forall ((p Policy))
    (= (Star p) (Union (Filter Tru) (Sequence p (Star p))))))

;;KA-LFP-L
(assert 
  (forall ((p Policy) (q Policy) (r Policy)) 
    (implies (Leq (Union q (Sequence p r)) r)
             (Leq (Sequence (Star p) q) r))))

;; KA-Unroll-R
(assert 
  (forall ((p Policy))
    (= (Star p) (Union (Filter Tru) (Sequence (Star p) p)))))

;;KA-LFP-R
(assert 
  (forall ((p Policy) (q Policy) (r Policy)) 
    (implies (Leq (Union p (Sequence q r)) q)
             (Leq (Sequence p (Star r)) q))))

;;;;;;;;;;;;;;;;;;;;
;; Packet Algebra ;;
;;;;;;;;;;;;;;;;;;;;

;; PA-Mod-Mod-Comm
(assert 
  (forall ((f1 Field) (v1 Value) (f2 Field) (v2 Value))
    (implies (not (= f1 f2))
             (= (Sequence (Filter (Test f1 v1)) (Filter (Test f2 v2)))
                (Sequence (Filter (Test f2 v2)) (Filter (Test f1 v1)))))))

;; PA-Mod-Filter-Comm
(assert 
  (forall ((f1 Field) (v1 Value) (f2 Field) (v2 Value))
    (implies (not (= f1 f2))
             (= (Sequence (Filter (Test f1 v1)) (Modify f2 v2))
                (Sequence (Modify f2 v2) (Filter (Test f1 v1)))))))

;; PA-Dup-Filter-Comm
;; (assert 
;;   (forall ((f Field) (v Value))
;;      (= (Sequence Dup (Filter (Test f v)))
;;         (Sequence (Filter (Test f v)) Dup))))

;; PA-Mod-Filter
(assert 
  (forall ((f Field) (v Value))
    (= (Sequence (Modify f v) (Filter (Test f v)))
       (Modify f v))))

;; PA-Filter-Mod
(assert 
  (forall ((f Field) (v Value))
    (= (Sequence (Filter (Test f v)) (Modify f v))
       (Filter (Test f v)))))

;; PA-Mod-Mod
(assert 
  (forall ((f Field) (v1 Value) (v2 Value))
    (= (Sequence (Modify f v1) (Modify f v2))
       (Modify f v2))))

;; PA-Contra
(assert 
  (forall ((f Field) (v1 Value) (v2 Value))
    (implies (not (= v1 v2))
       (Sequence (Filter (Test f v1)) (Filter (Test f v2)))
       (Filter Fls))))

;; PA-Match-All
(assert
  (forall ((f Field))
     (= (Union (Filter (Test f true)) (Filter (Test f false))) 
     (Filter Tru))))

;; Examples
;; ...

(check-sat)
(get-model)
