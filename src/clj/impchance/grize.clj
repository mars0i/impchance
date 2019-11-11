;; examples from Yves-Laurent Grize's dissertation:
;;
;; TOWARDS A STATIONARY CONTINUOUS LOWER PROBABILITY-BASED MODEL FOR
;; FLICKER NOISE
;; Yves-Laurent Grize
;; May 1984
;; Cornell University Dissertation
;; supervised by Terrence Fine

(ns impchance.grize
  (:use clojure.math.numeric-tower))

(def pos-ints (drop 1 (range)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example I.4.6, page 17:

;; Use this to find the value of 2^{2l}, then check whether the exponent is odd 
;; or even to decide which line of the definition of x-n-bar to use.
(defn lesser-power
  "Returns a sequence containing the (1) maximum value of base raised to a 
  positive integer <= n, and (2) the integer to which base was raised."
  [base n]
  (loop [prev-e -1 prev-power 0]
    (let [e (inc prev-e)
          power (expt base e)]
      (if (> power n)
        [prev-power prev-e] ; meaningless the first time
        (recur e power)))))

(defn i46-mean
  "Return the average from 1 to n as specified in example I.4.6 from 
  Grize's 1984 dissertation." 
  [n]
  (let [[pow exponent] (lesser-power 2 n)
        m (- n pow)
        s (if (even? exponent) 2 1)]
    (- s (/ m pow))))

(def i46-means-ratio
  "A lazy sequence of ratio averages from 1 to n as n increases, as 
  specified in example I.4.6 from Grize's 1984 dissertation.  These means 
  are calculated using the definition in the text rather than being 
  computed by averaging values from 1 to n."
  (map i46-mean pos-ints))

(def i46-means
  "A lazy sequence of float averages from 1 to n as n increases, as specified 
  in example I.4.6 from Grize's 1984 dissertation.  These means are calculated 
  using the definition in the text rather than being computed by averaging 
  values from 1 to n."
  (map (comp float i46-mean) pos-ints))

(defn elts-from-means
  "Generate a lazy sequence of values from 1 to n as n increases, defined 
  from a sequence of means as weighted differences between them.  See e.g.
  p. 17 in Grize's 1984 dissertation.  Also assumes that the first element
  should be the first mean, although Grize only defined elements after
  the first."
  [means]
  (let [weighted-means (map * pos-ints means)]
    (cons (first means)
          (map - (drop 1 weighted-means) weighted-means))))

(def i46-elts-ratio
  "A lazy sequence of ratio values as n increases, as specified by example 
  I.4.6 (p. 17) from Grize's 1984 dissertation, defined from a sequence 
  of means as weighted differences between them."
  (elts-from-means i46-means-ratio))

(def i46-elts 
  "A lazy sequence of float values as n increases, as specified by example 
  I.4.6 (p. 17) from Grize's 1984 dissertation, defined from a sequence 
  of means as weighted differences between them."
  (elts-from-means i46-means))

(defn allan-vars-from-means
  "Given a sequence of means, return the corresponding sequence of Allan
  variances, as in definition I.3.2 on page 10 of Grize's 1984 dissertation."
  [means]
  (map (fn [x-n x-2n]
         (let [d (- x-n x-2n)]
           (* 2 d d)))
       means
       (take-nth 2 means)))

(def i46-allan-vars-ratio 
  "A lazy sequence of ratio Allan-variances for the sequence defined in
  example I.4.6 (p. 17) from Grize's 1984 dissertation."
  (allan-vars-from-means i46-means-ratio))

(def i46-allan-vars 
  "A lazy sequence of float Allan-variances for the sequence defined in
  example I.4.6 (p. 17) from Grize's 1984 dissertation."
  (allan-vars-from-means i46-means))
