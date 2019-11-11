;; This has nothing to do with imprecise chance, but it was convenient to 
;; run from there.  These are plots to illustrate for students a point that 
;; McAllister makes in his 2003 paper "Algorithmic randomness in empirical data".

(ns impchance.mcallister
    (:use utils.plots
          utils.random))

(def rng  (make-rng))
(defn cos [x] (Math/cos (* 0.1 x)))

(def gs (repeatedly (fn [] (* 0.3 (next-gaussian rng)))))

;; make plot with cos and Guassian noise separately:
(def p (simple-plot (map cos (range 200))))
(simple-replot p (take 200 gs))

;; make plot with sum of cos and the same Guassian noise:
(simple-plot (map + gs (map cos (range 200))))

;; make plot with sum of cos and the same Guassian noise, and the
;; original cos function superimposed on it:
(def r (simple-plot (map + gs (map cos (range 200)))))
(simple-replot r (map cos (range 200)))
