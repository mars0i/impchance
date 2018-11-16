(ns impchance.core
  (require [impchance.random :as r]
           [impchance.random-utils :as ru])
  (:gen-class))

(def my-rng (r/make-rng))

(def my-ps [0.4 0.6])

(def my-top 1000)

(defn choose-length
  [rng top]
  (r/rand-idx rng top))

(defn choose-p
  [rng ps]
  (ru/sample-one rng ps))

(defn bernoulli
  [rng p]
  (if (< (r/next-double rng) p) 0 1))

(defn make-n-outcomes
  [rng top ps]
  (let [len (choose-length rng top)
        p (choose-p rng ps)]
    ;(println len p) ; DEBUg
    (repeatedly len (fn [] (bernoulli rng p)))))

;; Returns a lazy sequence concatenated from the sequences in the
;; sequence of sequences xss (which can be lazily-generated).
;; by OlegTheCat from https://stackoverflow.com/a/41618221/1455243
(defn apply-concat [xss]
  (lazy-seq
   (when-let [s (seq xss)]
     (concat (first s) (apply-concat (rest s))))))

(defn make-outcomes
  [rng top ps]
  (apply-concat (repeatedly (fn [] (make-n-outcomes rng top ps)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
    (println "Hello, World!\n")
    (println (take 1000 (make-outcomes my-rng my-top my-ps))))
