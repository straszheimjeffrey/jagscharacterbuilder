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

(ns jagsrpg.model
  (:use jls.dataflow.dataflow)
  (:use [clojure.contrib.except :only (throwf)])
  (:use [clojure.contrib.str-utils :only (re-split str-join)])
  (:use [clojure.contrib.math :only (round)]))

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

(def build-modifier-table
     (make-table 7 [-4 -2 -1 0 1 2 4 7 11 18 23 28 38]))

(defn compute-grapple-mod
  "Compute the grapple mod from multiple skills, mods is a collection
   of [mod, skill-level] pairs"
  [mods]
  (if (empty? mods)
    0
    (let [sorted (reverse (sort-by (fn [[mod level]]
                                     (- (* mod 5) level))) mods)
          [highest _] (first sorted)
          others (next sorted)
          step (fn [_ level]
                 (cond
                  (= level 2) 1
                  (= level 3) 2
                  (= level 4) 4))]
      (apply + highest (map step others)))))

(def walking-speed-table
     (make-table 7 [1 1 2 3 3 3 4 4 4 4 4 4]))
(def running-speed-table
     (make-table 7 [2 3 3 4 4 4 5 5 5 8 8 10]))
(def sprinting-speed-table
     (make-table 7 [3 4 5 6 6 6 7 7 7 10 10 12]))

(defn make-speed-function
  "Builds the funciton to compute speed"
  [table per-2-rea]
  (fn [n] (if (> n 18)
            (+ (table 18) (* per-2-rea (quot (- n 18) 2)))
            (table n))))

(def compute-walking-speed (make-speed-function walking-speed-table 4))
(def compute-running-speed (make-speed-function running-speed-table 2))
(def compute-sprinting-speed (make-speed-function sprinting-speed-table 2))

(defmacro max-secondary-mods
  "Ensure that the primary stat has only one secondary modified"
  [stat]
  (let [n (name stat)
        symb (symbol (str n "-secondary-mod"))]
    `(cell :validator (when (> (count ~(col-from-name symb)) 1)
                        (throwf Exception "Primary stat %s should only have one secondary stat modifier" ~n)))))

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
   (secondary-stat base-bld phy) ; for dp calculation
   (cell bld (apply + ?base-bld ?*bld-mods)) ; for damage bonus and other stuff
   (cell displayed-build (apply + ?bld ?*displayed-bld-mods)) ; This is to handle light
   (secondary-stat con phy)

   (secondary-stat cor ref)
   (secondary-stat rea ref)
   (secondary-stat agi ref)

   (secondary-stat res int)
   (secondary-stat mem int)
   (secondary-stat wil int)

   (cell base-damage (apply + (- ?str 10) (quot (- ?bld 10) 5) ?*base-damage-mods))
   (cell hand-to-hand-damage (apply + ?base-damage ?*hand-to-hand-damage-mods))

   (cell charm (apply + 10 ?*charm-mods))
   (cell intimidate (apply + 10 ?*intimidate-mods))
   (cell persuade (apply + 10 ?*persuade-mods))
   (cell recruit (apply + 10 ?*recruit-mods))

   (cell damage-points (apply + ?con (build-modifier-table ?base-bld) ?*damage-points-mods))

   (cell base-grapple (+ ?str (quot ?bld 5)))
   (cell defensive-grapple (apply + ?base-grapple ?*defensive-grapple-mods))
   (cell offensive-grapple (let [base (apply + ?base-grapple ?*offensive-grapple-mods)]
                             (max (+ base 2) (round (* base 1.2)))))

   (cell defensive-grapple-mods (compute-grapple-mod ?*defensive-grapple-skill-mods))
   (cell offensive-grapple-mods (compute-grapple-mod ?*offensive-grapple-skill-mods))

   (cell walking-ground-speed (apply + (compute-walking-speed ?rea) ?*walking-ground-speed-mods))
   (cell running-ground-speed (apply + (compute-running-speed ?rea) ?*running-ground-speed-mods))
   (cell sprinting-ground-speed (apply + (compute-sprinting-speed ?rea) ?*sprinting-ground-speed-mods))

   (cell initiative (apply + ?rea ?*initiative-mods))
   
   (cell perception (apply + ?res ?*perception-mods))

   ;; Ensure the max. number of secondary stat mods are respected
   (cell :validator (when (> (count ?*secondary-stat-modifier) 3)
                      (throwf Exception "Too many secondary stat modifiers: max 3")))
   (max-secondary-mods phy)
   (max-secondary-mods ref)
   (max-secondary-mods int)
])   

;;; Traits and skills

;; A trait factory makes a trait or skill instance
(defstruct trait-factory
  :name    ; The name, a String
  :make)   ; Creates an instance -- no parameters

;; A modifiable source cell
(defstruct modifiable
  :name       ; For display, a String
  :range      ; [min, max] integers
  :cell)      ; A source cell

;; A modifier of a secondary trait
(defstruct secondary-stat-trait
  :name          ; The name, a String
  :modifiables   ; A collection of modifiables
  :add           ; A function of one argument, to add this to a character model
  :remove)       ; The same, but removes this

(def secondary-stat-cost-table
     (make-table 8 [1 2 2 2 3 5 7 8 9 10 11 12 13]))

(defn secondary-cost
  "Compute the standard secondary cost"
  [level primary mult]
  (apply + (for [i (range 1 (inc level))]
             (let [val (+ primary (* i mult))]
               (secondary-stat-cost-table val)))))

(defn secondary-display-name
  "Converts name such as fat-obese to Fat/Obese"
  [name]
  (str-join "/" (map #(apply str (Character/toUpperCase (first %)) (next %))
                     (re-split #"-" (str name)))))

(defmacro standard-secondary-stat-trait
  "A trait-factory to build a standard secondary trait modifier.
   Direction is :increase or :decrease.  If display-name is nil, it
   will equal the cell-name"
  [cell-name secondary primary direction]
  (let [display-name (secondary-display-name cell-name)
        multiplier (cond
                    (= direction :increase) 1
                    (= direction :decrease) -1
                    :otherwise (throwf Exception "Bad direction %s" (str :direction)))
        modifier-name (symbol (str (name secondary) "-mods"))
        val2-name (symbol (str (name primary) "-secondary-mod"))]
    `(struct-map trait-factory
         :name ~display-name
         :make (fn []
                 (let [main-cell# (cell :source ~cell-name 1)
                       cost-cell# (cell ~'cp-cost (secondary-cost ~(var-from-name cell-name)
                                                                ~(var-from-name primary)
                                                                ~multiplier))
                       mod-cell# (cell ~modifier-name (* ~(var-from-name cell-name) ~multiplier))
                       val1-cell# (cell ~'secondary-stat-modifier (quote ~cell-name))
                       val2-cell# (cell ~val2-name (quote ~cell-name))
                       val3-cell# (cell :validator (when (or (< ~(var-from-name cell-name) 1)
                                                             (> ~(var-from-name cell-name) 2))
                                                      (throwf Exception "%s is out of range" ~display-name)))
                       cells# [main-cell# cost-cell# mod-cell# val1-cell# val2-cell# val3-cell#]]
                   (struct-map secondary-stat-trait
                     :name ~display-name
                     :modifiables [(struct  modifiable "Level" [1 2] main-cell#)]
                     :add (fn [char#]
                            (add-cells char# cells#))
                     :remove (fn [char#]
                               (remove-cells char# cells#))))))))
       

(comment

(def powerful ((:make (standard-secondary-stat-trait powerful str phy :increase))))

(def fred (build-dataflow (build-main-character-model)))
(print-dataflow fred)

((:add powerful) fred)

(update-values fred {'powerful 2})
(add-cells fred [(cell ap-cost 5)])
(remove-cells fred (get-cells fred 'ap-cost))

(use :reload 'jagsrpg.model)
(use :reload 'jls.dataflow.dataflow)
(use 'clojure.contrib.stacktrace) (e)
)

;; End of file
