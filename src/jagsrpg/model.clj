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
  (:use jagsrpg.utilities)
  (:use jagsrpg.damage)
  (:use clojure.contrib.dataflow)
  (:use [clojure.contrib.except :only (throwf)])
  (:use [clojure.contrib.seq-utils :only (find-first)])
  (:use [clojure.contrib.math :only (round floor)]))


;;; Modifiables

;; A modifiable source cell
(defstruct modifiable
  :name       ; For display, a String
  :range      ; [min, max] integers
  :cell       ; A source cell
  :validator) ; A validator cell

(defmacro make-modifiable
  "Create a modifiable instance"
  [symb range start]
  (let [display-name (make-display-name symb)
        var-name (var-from-name symb)]
    `(struct-map modifiable
         :name ~display-name
         :range ~range
         :cell (cell :source ~symb ~start)
         :validator (cell :validator
                          (when (or (< ~var-name ~(first range))
                                    (> ~var-name ~(second range)))
                            (throwf "%s is out of range %s" ~display-name ~range))))))

(defn get-modifiable-cells
  "Get the cells from a modifiable"
  [modifiable]
  [(:cell modifiable) (:validator modifiable)])


;;; Primary Model

(defmacro secondary-stat
  "Builds a secondary stat"
  [stat primary]
  (let [n (name stat)
        mods (symcat n "-mods")]
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
  (let [cost-name (symcat stat "-cost")]
    `(cell ~cost-name (primary-stat-cost-table ~(var-from-name stat)))))

(def build-modifier-table
     (make-table 7 [-4 -2 -1 0 1 2 4 7 11 18 23 28 38]))

(defn compute-grapple-mod
  "Compute the grapple mod from multiple skills, mods is a collection
   of [mod, skill-level] pairs"
  [mods]
  (if (empty? mods)
    0
    (let [sorted (reverse (sort-by (fn [[mod level]]
                                     (- (* mod 5) level)) mods))
          [highest _] (first sorted)
          others (next sorted)
          step (fn [[_ level]]
                 (cond
                  (= level 2) 1
                  (= level 3) 2
                  (= level 4) 4
                  :otherwise 0))]
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
        symb (symcat n "-secondary-mod")]
    `(cell :validator
           (when (> (count ~(col-from-name symb)) 1)
             (throwf Exception
                     "Primary stat %s should only have one secondary stat modifier"
                     ~n)))))

(defstruct jags-character
  :primary-stats    ; The modifiables making the primary stats
  :model            ; The dataflow model
  :traits)          ; A ref to a collection of traits

(defn get-primary-stat
  "Get a primary stat by its symbolic name"
  [ch name]
  (let [stats (:primary-stats ch)
        match (fn [m]
                (= name (-> m :cell :name)))]
    (find-first match stats)))

(defmacro basic-damage-charts
  []
  (vec (concat (impact-chart 'basic-punch)
               (impact-chart 'basic-cross)
               (impact-chart 'basic-kick))))

(defn build-character
  "Returns a jags-character"
  []
  (let [primaries [(make-modifiable phy [8 20] 10)
                   (make-modifiable ref [8 20] 10)
                   (make-modifiable int [8 20] 10)]
        model [(cell :source name "-- name --")

               (cell total-cp-cost (apply + ?*cp-cost))
               (cell total-ap-cost (apply + ?*ap-cost))

               (cell cp-cost (apply + ?*cp-cost-mods))
               (cell ap-cost (apply + ?*ap-cost-mods))

               (primary-stat-cost phy)
               (primary-stat-cost ref)
               (primary-stat-cost int)
               (cell cp-cost ?phy-cost)
               (cell cp-cost ?ref-cost)
               (cell cp-cost ?int-cost)

               (secondary-stat str phy)
               ; for dp calculation
               (secondary-stat base-bld phy)
               ; for damage bonus and other stuff
               (cell bld (apply + ?base-bld ?*bld-mods))
               ; This is to handle light 
               (cell displayed-bld (apply + ?bld ?*displayed-bld-mods))
               (secondary-stat con phy)

               (secondary-stat cor ref)
               (secondary-stat rea ref)
               (secondary-stat agi ref)

               (secondary-stat res int)
               (secondary-stat mem int)
               (secondary-stat wil int)

               ; for artistic genius
               (cell mem-for-art (apply + ?res ?*mem-for-art-mods))

               (cell base-damage
                     (apply + (- ?str 10) (quot (- ?bld 10) 5) ?*base-damage-mods))
               (cell hand-to-hand-damage
                     (apply + ?base-damage ?*hand-to-hand-damage-mods))

               (cell basic-punch (apply + ?hand-to-hand-damage ?*basic-punch-mods))
               (cell basic-cross (apply + (j-add* ?hand-to-hand-damage 1)
                                          ?*basic-cross-mods))
               (cell basic-kick (apply + (j-add* ?hand-to-hand-damage 2)
                                         ?*basic-kick-mods))

               (cell charm (apply + 10 ?*charm-mods))
               (cell intimidate (apply + 10 ?*intimidate-mods))
               (cell persuade (apply + 10 ?*persuade-mods))
               (cell recruit (apply + 10 ?*recruit-mods))

               (cell damage-points
                     (apply + ?con
                              (build-modifier-table ?base-bld)
                              ?*damage-points-mods))

               (cell base-grapple (+ ?str (quot ?bld 5)))
               (cell defensive-grapple (apply + ?base-grapple ?*defensive-grapple-mods))
               (cell offensive-grapple
                     (let [base (apply + ?base-grapple ?*offensive-grapple-mods)]
                       (max (+ base 2) (round (* base 1.2)))))

               (cell defensive-grapple-mods
                     (compute-grapple-mod ?*defensive-grapple-skill-mods))
               (cell offensive-grapple-mods
                     (compute-grapple-mod ?*offensive-grapple-skill-mods))

               (cell rea-for-speed (apply + ?rea ?*rea-for-speed-mods))

               (cell walking-ground-speed
                     (apply + (compute-walking-speed ?rea-for-speed)
                              ?*walking-ground-speed-mods))
               (cell running-ground-speed
                     (apply + (compute-running-speed ?rea-for-speed)
                              ?*running-ground-speed-mods))
               (cell sprinting-ground-speed
                     (apply + (compute-sprinting-speed ?rea-for-speed)
                              ?*sprinting-ground-speed-mods))

               (cell initiative (apply + ?rea ?*initiative-mods))
   
               (cell perception (apply + ?res ?*perception-mods))

               (cell armor-dr (apply + ?*armor-dr-mods))
               (cell armor-pen (apply + ?*armor-pen-mods))

               (cell minor-wound-level (apply + (round (/ ?damage-points 3))
                                                ?*minor-wound-level-mods))
               (cell major-wound-level (apply + ?damage-points ?*major-wound-level-mods))
               (cell critical-wound-level (apply + (* 2 ?damage-points)
                                                   ?*critical-wound-level-mods))

               (cell hurt-condition (apply + (round (/ ?damage-points 3))
                                             ?*hurt-condition-mods))
               (cell injured-condition (apply + ?damage-points ?*injured-condition-mods))
               (cell serious-condition (apply + (* 2 ?damage-points)
                                                ?*serious-condition-mods))

               (cell agi-bonus (apply + (- ?agi 10) ?*agi-bonus-mods))
               (cell agi-bonus-hth (apply + ?agi-bonus ?*agi-bonus-hth-mods))
               (cell agi-bonus-ranged (apply + (floor (/ ?agi-bonus 2))
                                               ?*agi-bonus-ranged-mods))

               (cell :source eye-color "")
               (cell :source hair-color "")
               (cell :source gender "")
               (cell :source height "")
               (cell :source description "")
               (cell :source background "")

               ;; Ensure the max. number of secondary stat mods are respected
               (cell :validator
                     (when (> (count ?*secondary-stat-modifier) 3)
                       (throwf Exception
                               "Too many secondary stat modifiers: max 3")))
               (max-secondary-mods phy)
               (max-secondary-mods ref)
               (max-secondary-mods int)]
        damage (basic-damage-charts)
        traits (ref #{})]
    (struct-map jags-character
      :primary-stats primaries
      :model (build-dataflow (concat (mapcat get-modifiable-cells
                                             primaries)
                                     damage
                                     model))
      :traits traits)))


;;; Traits and skills

;; A trait factory makes a trait or skill instance
(defstruct trait-factory
  :name    ; The name, a String
  :make)   ; Creates an instance -- no parameters

;; A trait
(defstruct trait
  :name          ; The name, a String -- must match its factory name
  :symb-name     ; The name as a symbol
  :type          ; Such as :secondary, :trait, :archetype, or :skill
  :modifiables   ; A collection of modifiables
  :cost          ; The cost cell, can be nil
  :notes         ; The notes cell, holds a string
  :cells)        ; All cells of this trait

(defn add-traits
  "Add trait to character"
  [ch trs]
  (dosync (alter (:traits ch) (fn [s] (apply conj s trs)))
          (add-cells (:model ch) (mapcat :cells trs))))

(defn remove-traits
  "Remove a trait from a character"
  [ch trs]
  (dosync (alter (:traits ch) (fn [s] (apply disj s trs)))
          (remove-cells (:model ch) (mapcat :cells trs))))

(defmacro basic-trait
  "Create a basic trait factory.  cost is a cell, usually defining a
   cp-cost or ap-cost.  Cells is a collection for arbitrary cells."
  [trait-name type cost cells]
  `(struct-map trait-factory
     :name ~(make-display-name trait-name)
     :make (fn []
             (let [cost# ~cost
                   notes# (cell :source ~(symcat trait-name "-notes") "")
                   cells# (conj ~cells cost# notes#)]
               (struct-map trait
                 :name ~(make-display-name trait-name)
                 :symb-name (quote ~trait-name)
                 :type ~type
                 :modifiables [(make-modifiable mod# [1 1] 1)]
                 :cost cost#
                 :notes notes#
                 :cells cells#)))))

(defmacro variable-trait
  "A trait that can vary according to a source."
  [trait-name type modifiable cost cells]
  `(struct-map trait-factory
     :name ~(make-display-name trait-name)
     :make (fn []
             (let [mod# ~modifiable
                   cost# ~cost
                   notes# (cell :source ~(symcat trait-name "-notes") "")]
               (struct-map trait
                 :name ~(make-display-name trait-name)
                 :symb-name (quote ~trait-name)
                 :type ~type
                 :modifiables [mod#]
                 :cost cost#
                 :notes notes#
                 :cells (concat ~cells (get-modifiable-cells mod#) [cost# notes#]))))))

;;; Trait

(defmacro trait-base
  "Define a standard trait"
  ([type trait-name cost-vec]
     `(trait-base ~type ~trait-name ~cost-vec nil))
  ([type trait-name cost-vec cells]
     (let [cost-name (symcat trait-name "-cost")]
       `(variable-trait ~trait-name
                        ~type
                        (make-modifiable ~trait-name [1 ~(count cost-vec)] 1)
                        (cell ~(condp = type
                                        :trait 'cp-cost
                                        :archetype 'ap-cost)
                              (~cost-vec (dec ~(var-from-name trait-name))))
                        ~cells))))

;;;;;;
(comment

(def fred (build-character))
(print-dataflow (:model fred))

(use :reload 'jagsrpg.model)
(use 'clojure.contrib.stacktrace) (e)
(use 'clojure.contrib.trace)
)

;; End of file
