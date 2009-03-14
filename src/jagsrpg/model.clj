;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  model.clj
;;
;;  The core model of a Jags Character
;;
;;  straszheimjeffrey (gmail)
;;  Created 14 March 2009

(ns jagsrpg.model.clj
  (:use jls.dataflow.dataflow))

(defn var-from-name
  "Given a symbol x, return ?x"
  [symb]
  (symbol (str "?" (name symb))))

(defn col-from-name
  "Given a symbol x, return ?*x"
  [symb]
  (symbol (str "?*" (name symb))))

(defmacro secondary-stat
  "Builds a secondary stat"
  [stat primary]
  (let [n (name stat)
        mods (symbol (str n "-mods"))]
    `(cell ~stat (apply + ~(var-from-name primary) ~(col-from-name mods)))))

(defn make-table
  "Returns a lookup table starting at start with values in values"
  [start values]
  (fn [n] (nth values (- n start))))

(def primary-stat-cost-table
     (make-table 8 [-8 -5 0 5 15 30 50 75 105 140 180 255 275]))

(defmacro primary-stat-cost
  "Builds a cell for the cost of a primary stat"
  [stat]
  `(cell ~'cp-cost (primary-stat-cost-table ~(var-from-name stat))))

(defn build-main-character-model
  "Returns a collection of cells defining the core model of a JAGS character"
  []
  [(cell total-cp-cost (apply + ?*cp-cost))
   (cell total-ap-cost (apply + ?*ap-cost))

   (cell :source phy 10)
   (cell :source ref 10)
   (cell :source int 10)

   (primary-stat-cost phy)
   (primary-stat-cost ref)
   (primary-stat-cost int)

   (secondary-stat str phy)
   (secondary-stat bld phy)
   (secondary-stat con phy)

   (secondary-stat cor ref)
   (secondary-stat rea ref)
   (secondary-stat agi ref)

   (secondary-stat res int)
   (secondary-stat mem int)
   (secondary-stat wil int)

   (cell base-damage (apply + (- ?str 10) (quot (- ?bld 10) 5) ?*base-damage-mods))

   (cell charm (apply + 10 ?*charm-mods))
   (cell intimidate (apply + 10 ?*intimidate-mods))
   (cell persuade (apply + 10 ?*persuade-mods))
   (cell recruit (apply + 10 ?*recruit-mods))
])   

(comment
(def fred (build-dataflow (build-main-character-model)))
(print-dataflow fred)
(update-values fred {'phy 15 'int 12})
(add-cells fred [(cell ap-cost 5)])
(remove-cells fred (get-cells fred 'ap-cost))
)

;; End of file
