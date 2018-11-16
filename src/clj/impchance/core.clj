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
    (println len p) ; DEBUg
    (repeatedly len (fn [] (bernoulli rng p)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
    (println "Hello, World!\n")
    (println (make-n-outcomes my-rng my-top my-ps)))
