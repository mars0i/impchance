;; examples from Yves-Laurent Grize's dissertation

(ns impchance.grize
  (:use clojure.math.numeric-tower))

;; Example I.4.6

;; Use this to find the value of 2^{2l}, then check whether the exponent is odd 
;; or even to decide which line of the definition of x-n-bar to use.
(defn lesser-power
  "Returns a sequence containing the maximum value of base raised to a positive 
  integer that's <= n and the integer to which base was raised."
  [base n]
  (loop [prev-e -1 prev-power 0]
    (let [e (inc prev-e)
          power (expt base e)]
      (if (> power n)
        [prev-power e]
        (recur e power)))))

(defn i46
  "Return the average from 1 to n as specified in example I.4.6."
  [n]
  (let [[pow exponent] (lesser-power 2 n)
        m (- n pow)
        s (if (even? exponent) 2 1)]
    (float (- s (/ m pow)))))
