(set-option :auto-config false)
;; Values
(define-sort Value () (_ BitVec 32))


;; Datatype for locations, pairs of switch and port
(declare-datatypes () ((Location disconnected (mk-loc (switch Value) (port Value)))))


;; Encode a topology as a map from locations to an array of (connected) locations
(define-sort Topology  () (Array Location Location))

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

;; Define the adjacency function on the topology representation
;; s1:p2 -- s2:p1
;; s1:p3 -- s3:p2
(declare-const t1 Topology)

(assert (= (select t1 (mk-loc #x00000001 #x00000002))
           (mk-loc #x00000002 #x00000001)))
(assert (= (select t1 (mk-loc #x00000001 #x00000003))
           (mk-loc #x00000003 #x00000001)))
(assert (forall ((l1 Location))
                (or
                 (or (= l1 (mk-loc #x00000003 #x00000001))
                     (= l1 (mk-loc #x00000001 #x00000003))
                     (= l1 (mk-loc #x00000002 #x00000001))
                     (= l1 (mk-loc #x00000001 #x00000002)))
                 (= (select t1 l1) disconnected))))

(assert (adjacent t1 (mk-loc #x00000001 #x00000002) (mk-loc #x00000002 #x00000001)))
(assert (adjacent t1 (mk-loc #x00000003 #x00000001) (mk-loc #x00000001 #x00000003)))

(assert (adjacent t1 (mk-loc #x00000003 #x00000001) (mk-loc #x00000001 #x00000002)))
(assert (not (adjacent t1 (mk-loc #x00000003 #x00000002) (mk-loc #x00000002 #x00000003))))


(check-sat)

