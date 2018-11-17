(ns impchance.core
  (require [impchance.random :as r]
           [impchance.random-utils :as ru])
  (:gen-class))

(def my-rng (r/make-rng))

(def my-ps [0.4 0.6])

(def my-top 1000)

(defn choose-length
  "Randomly chooses an integer between 1 and top, inclusive, with equal
  probability for each integer." 
  [rng top]
  (inc (r/rand-idx rng top)))

(defn choose-p
  "Randomly chooses an element from ps with equal probability for each element."
  [rng ps]
  (ru/sample-one rng ps))

(defn bernoulli
  "Returns a 0 or 1 with probability p for 1."
  [rng p]
  (if (< (r/next-double rng) p) 1 0))

(defn make-n-outcomes
  "Returns a sequence of n 0's and 1's with a probability randomly chosen
  from ps for all n trials, where n is a random integer between 1 and top,
  inclusive."
  [rng top ps]
  (let [len (choose-length rng top)
        p (choose-p rng ps)]
    ;(println len p) ; DEBUg
    (repeatedly len (fn [] (bernoulli rng p)))))

;; by OlegTheCat from https://stackoverflow.com/a/41618221/1455243
(defn apply-concat [xss]
  "Returns a lazy sequence concatenated from the sequences in the
  sequence of sequences xss (which can be lazily-generated)."
  (lazy-seq
   (when-let [s (seq xss)]
     (concat (first s) (apply-concat (rest s))))))

(defn make-outcomes
  "Return a lazy sequence of outcomes repeatedly generated by make-n-outcomes."
  [rng top ps]
  (apply-concat (repeatedly (fn [] (make-n-outcomes rng top ps)))))

(defn make-os
  "Convenience version of make-outcomes for testing.  Uses my-rng, my-top,
  and my-ps."
  []
  (make-outcomes my-rng my-top my-ps))

(defn relative-frequency
  "General function for calculating the relative frequency of elements
  that satisfy pred."
  [pred os]
  (float
    (/ (reduce (fn [count-so-far x]
                 (if (pred x)
                   (inc count-so-far)
                   count-so-far))
               0 os)
       (count os))))

(defn relf
  "Calculate relative frequency of 1 in seq consisting only of 0's and 1s."
  [os]
  (float
    (/ (reduce + os)
       (count os))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
    (println "Hello, World!\n")
    (println (take 1000 (make-outcomes my-rng my-top my-ps))))
