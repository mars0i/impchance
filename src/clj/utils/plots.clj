;;; This software is copyright 2015, 2019 by Marshall Abrams, and
;;; is distributed under the Gnu General Public License version 3.0 as
;;; specified in the file LICENSE.

;; NOTE: These plots use JFreeChart via Incanter.  When the mean is too
;; close to 0 or 1, the chart becomes non-sensical; the y ticks disappear,
;; and nothing is plotted.  You can still make a plot e.g. with R in this
;; situation.

;; Other useful things:
;; (use '[incanter.pdf])
;; (save-pdf xyp filename)
;; where xyp is a plot object.

(ns utils.plots
  (:require [incanter.core :as ic]
            [incanter.charts :as ich]))

(defn simple-replot
  "Given plot object xyp, adds the values in ys in relation to x values from 0
  to the length of ys, displays the modified plot object, and returns it.  If
  n is provided, no more than the first n elements of ys will be used."
  ([xyp ys] (let [xs (range (count ys))]
              (ic/view xyp)
              (ich/add-lines xyp xs ys)))
  ([xyp n ys] (simple-replot xyp (take n ys))))

(defn simple-plot
  "Displays a line plot of the values in ys in relation to x values from 
  0 to the length of ys, and returns the plot object.  A new xy-plot object
  is generated and returned.  If n is provided, no more than the first n
  elements of ys will be used."
  ([ys] (simple-replot (ich/xy-plot) ys))
  ([n ys & config-params]
    (simple-replot (apply ich/xy-plot config-params) n ys)))

(defn mean
  [xs]
  (/ (reduce + xs) (count xs)))
