;; This software is copyright 2019 by Marshall Abrams, 
;; and is distributed under the Gnu General Public License version 3.0 
;; as specified in the the file LICENSE.

;(set! *warn-on-reflection* true)

(ns utils.pink
  (:require [utils.random :as r]
            [clojure.math.numeric-tower :as math])
  (:import PinkNoiseFast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core functions

(defn make-pink
  "Return a PinkNoise PRNG based on ec.util.MersenneTwisterFast, with
  specified alpha (the power to which 1/f is raised), and poles (which
  provide a way of specifying how long the partial sum will be).  
  If not supplied, the internal rng will be a MersenneTwisterFast object.  
  See PinkNoise.java or 
  http://sampo.kapsi.fi/PinkNoise for more info about alpha and poles."
  ([alpha poles] (make-pink (r/make-rng) alpha poles))
  ([rng alpha poles] (PinkNoiseFast. alpha poles rng)))

;; Not using next-double for this function since the next-double function
;; in Random.java and MersenneTwisterFast.java is restricted to [0,1).
(defn next-value
  "Returns a random double from a pink-noise PRNG.  Numbers are not 
  restricted to any particular range, and could be negative."
  [pink]
  (.nextValue pink))

;(defn logistic
;  "Standard logistic function."
;  [midpoint maximum growth x]
;  (/ maximum (inc (java.lang.Math/exp (* growth (- midpoint x))))))

(defn normalizing-logistic
  "Logistic function with max value 1, mid x value 0, and user-specified
  growth rate, which specifies how steep the curve is.  Maps [-inf,inf]
  into [0,1].  (The range is a closed interval because doubles are not
  arbitrary-precision.)"
  [growth x]
  (/ (inc (java.lang.Math/exp (- (* growth x))))))

;; Related to the next-double function in Random.java and 
;; MersenneTwisterFast.java, but this takes an additional argument that
;; can be used to define a particular pink noise variant of next-double
;; using partial.
(defn next-normalized-double
  "Returns a random double in the half-open range from [0.0,1.0) from a 
  pink-noise PRNG normalized by normalizing-logistic.  growth should be
  non-negative, and specifies how steep the logistic curve is; larger
  values make it steeper, i.e. larger values will push inputs near 0.5
  more toward 0 or 1."
  [growth pink]
  (normalizing-logistic growth (next-value pink)))

(defn rand-idx
  "Return an integer in [0,sup) distributed as if it is an integer version
  of the output of next-normalized-double (which uses normalizing-logistic)."
  [pink growth sup]
  (long (math/floor
          (* sup (next-normalized-double growth pink)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience functions for experimentation

(defn pink-nums
  "Return a lazy sequence of numbers generated by PinkNoise object pink."
  [pink] (repeatedly (partial next-normalized-double pink)))

(defn logistic-pink-nums
  "Return a lazy sequence of numbers generated by PinkNoise object pink 
  and then restricted to [0,1] by passing them through normalizing-logistic 
  with the specified growth-rate."
  [pink growth-rate] (repeatedly (comp (partial normalizing-logistic growth-rate)
                                       (partial next-normalized-double pink))))

(defn normalize
  "Applies mod to all numbers in xs, wrapping their values into [0,n).
  Numbers >= n are mapped into the number above 0 after wrapping around up
  as many times as necessary.  Negative numbers are wrapped around in the 
  other direction.  For example, (normalize 1 xs) will map -2.3 to 0.7 ."
  [n xs]
  (map (fn [x] (mod x n)) xs))

;; for experiments with pink noise
(defn extrema
  "Returns [min max] of xs."
  ([xs] (let [xs-first (first xs)
             xs-rest (rest xs)]
         (reduce 
           (fn [[mn mx] x]
             [(min mn x) (max mx x)])
           [xs-first xs-first]
           xs-rest)))
  ([n xs] (extrema (take n xs))))

(defn mean
  [xs]
  (/ (reduce + xs) (count xs)))
