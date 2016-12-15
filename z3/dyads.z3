;; cij is true if policy dyad i is compatible with fabric dyad j
(declare-var c00 Bool)
(declare-var c01 Bool)
(declare-var c02 Bool)
(declare-var c10 Bool)
(declare-var c11 Bool)
(declare-var c12 Bool)

;; vij is true if policy dyad i is implemented using fabric dyad j
(declare-var v00 Bool)
(declare-var v01 Bool)
(declare-var v02 Bool)
(declare-var v10 Bool)
(declare-var v11 Bool)
(declare-var v12 Bool)

;; For each i, pick only one j
(assert (or (and v00 (not v01) (not v02))
            (and (not v00) v01 (not v02))
            (and (not v00) (not v01) v02)))
(assert (or (and v10 (not v11) (not v12))
            (and (not v10) v11 (not v12))
            (and (not v10) (not v11) v12)))


;; vij is true if cij is
(assert (implies v00 c00))
(assert (implies v01 c01))
(assert (implies v02 c02))
(assert (implies v10 c10))
(assert (implies v11 c11))
(assert (implies v12 c12))

;; Write compatibilities
(assert (not c00))
(assert c01)
(assert c02)
(assert c10)
(assert c11)
(assert (not c12))

(check-sat)
(get-model)
