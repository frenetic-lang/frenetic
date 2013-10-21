;; Define a type Set using the Array theory

;; We put the actual elements in the codomain of the Array (so we can map on them) and use -1 to represent an element not present.

(declare-datatypes 
 () 
 ((Packet 
   (nopacket )
   (packet 
    (Switch Int) 
    (EthDst Int) 
    (EthType Int) 
    (Vlan Int) 
    (VlanPcp Int) 
    (IPProto Int) 
    (IP4Src Int) 
    (IP4Dst Int) 
    (TCPSrcPort Int) 
    (TCPDstPort Int) 
    (EthSrc Int) 
    (InPort Int)))))

(define-sort Set () (Array Packet Packet))

(define-fun set_empty () Set ((as const Set) nopacket))

(define-fun set_mem ((x Packet) (s Set)) Bool (not (= (select s x) nopacket)))

(define-fun set_add ((x Packet) (s Set)) Set (store s x x))

;; handy macro (C macro) for defining functions.

(declare-fun this-works (Int) Int)
(assert (forall ((x Int)) (= (this-works x) (+ x 2))))
(check-sat)

#define SFUN(name,argname,body) (declare-fun name (Set) Set) \
(assert (forall ((argname Set)) (= (name argname) body)))

#define PFUN2(name,argname1,argname2,body) (declare-fun name (Packet Packet) Packet) \
(assert (forall ((argname1 Packet) (argname2 Packet)) (= (name argname1 argname2) body)))

#define SFUN2(name,argname1,argname2,body) (declare-fun name (Set Set) Set) \
(assert (forall ((argname1 Set) (argname2 Set)) (= (name argname1 argname2) body)))

#define SFUN2ret(name,argname1,argname2,rettype, body) (declare-fun name (Set Set) rettype) \
(assert (forall ((argname1 Set) (argname2 Set)) (= (name argname1 argname2) body)))

#define fresh_packet(pkt) (declare-var pkt Packet) \
(declare-var pkt-int1-__LINE__ Int) \
(declare-var pkt-int2-__LINE__ Int) \
(declare-var pkt-int3-__LINE__ Int) \
(declare-var pkt-int4-__LINE__ Int) \
(declare-var pkt-int5-__LINE__ Int) \
(declare-var pkt-int6-__LINE__ Int) \
(declare-var pkt-int7-__LINE__ Int) \
(declare-var pkt-int8-__LINE__ Int) \
(declare-var pkt-int9-__LINE__ Int) \
(declare-var pkt-int10-__LINE__ Int) \
(declare-var pkt-int11-__LINE__ Int) \
(declare-var pkt-int12-__LINE__ Int) \
(assert (= pkt  \
	   (packet pkt-int11-__LINE__ pkt-int10-__LINE__ pkt-int9-__LINE__  \
		   pkt-int8-__LINE__ pkt-int7-__LINE__ pkt-int6-__LINE__  \
		   pkt-int5-__LINE__ pkt-int12-__LINE__ pkt-int4-__LINE__  \
		   pkt-int3-__LINE__ pkt-int2-__LINE__ pkt-int1-__LINE__)))


;; end macros

SFUN(h,x,x)
PFUN2(packet-or, x, y, (ite (= x nopacket) y x))
PFUN2(packet-and, x, y, (ite (and (not (= x nopacket)) (= x y)) x nopacket))
PFUN2(packet-diff, x, y, (ite (= x y) nopacket x))
SFUN2(set-inter, s1, s2, ((_ map packet-and) s1 s2))
SFUN2(set-union, s1, s2, ((_ map packet-or) s1 s2))
SFUN2(set-diff, s1, s2, ((_ map packet-diff) s1 s2))
SFUN2ret(set-subseteq, s1, s2, Bool, (= set_empty (set-diff s1 s2)))

PFUN2(convenient-map-test, x, y, (ite (= (Switch x) (+ 3 (Switch y))) y nopacket ))

(push)
(declare-var set1 Set)
(declare-var set2 Set)
(assert (= ((_ map convenient-map-test) set1 set2) set2))

(check-sat)
(pop)

fresh_packet(pkt1)
(assert (= (packet-or pkt1 nopacket) pkt1 ))
(assert (not (= pkt1 nopacket)))
(check-sat)


;; Now lift g to a function from Sets to Sets

(define-sort DblSet () (Array Packet Set))

(check-sat)
