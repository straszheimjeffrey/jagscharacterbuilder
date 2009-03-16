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
  (:use clojure.contrib.dataflow)
  (:use [clojure.contrib.except :only (throwf)])
  (:use [clojure.contrib.str-utils :only (re-split str-join)])
  (:use [clojure.contrib.seq-utils :only (seek)])
  (:use [clojure.contrib.math :only (round)]))

(defn var-from-name
  "Given a symbol x, return ?x"
  [symb]
  (symbol (str "?" (name symb))))

(defn col-from-name
  "Given a symbol x, return ?*x"
  [symb]
  (symbol (str "?*" (name symb))))

(defn make-display-name
  "Converts name such as fat-obese to Fat/Obese"
  [name]
  (str-join " " (map #(apply str (Character/toUpperCase (first %)) (next %))
                     (re-split #"-" (str name)))))

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

(defn add-modifiable
  "Adds the cell and a validator to a model"
  [model modifiable]
  (add-cells model [(:cell modifiable) (:validator modifiable)]))

(defn remove-modifiable
  "Removes the cell and validator from a model"
  [model modifiable]
  (remove-cells model [(:cell modifiable) (:validator modifiable)]))

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
  (let [cost-name (symbol (str (name stat) "-cost"))]
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
        symb (symbol (str n "-secondary-mod"))]
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
    (seek match stats)))
  
(defn build-character
  "Returns a jags-character"
  []
  (let [primaries [(make-modifiable phy [8 20] 10)
                   (make-modifiable ref [8 20] 10)
                   (make-modifiable int [8 20] 10)]
        model [(cell :source name "-- name --")

               (cell total-cp-cost (apply + ?*cp-cost))
               (cell total-ap-cost (apply + ?*ap-cost))

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

               ;; Ensure the max. number of secondary stat mods are respected
               (cell :validator
                     (when (> (count ?*secondary-stat-modifier) 3)
                       (throwf Exception
                               "Too many secondary stat modifiers: max 3")))
               (max-secondary-mods phy)
               (max-secondary-mods ref)
               (max-secondary-mods int)]
        traits (ref #{})]
    (struct-map jags-character
      :primary-stats primaries
      :model (build-dataflow (concat (map :cell primaries)
                                     (map :validator primaries) model))
      :traits traits)))


;;; Traits and skills

;; A trait factory makes a trait or skill instance
(defstruct trait-factory
  :name    ; The name, a String
  :make)   ; Creates an instance -- no parameters

;; A trait
(defstruct trait
  :name          ; The name, a String
  :modifiables   ; A collection of modifiables
  :cost          ; The cost cell
  :add           ; A function of one argument, to add this to a character model
  :remove)       ; The same, but removes this

(defn add-trait
  "Add trait to character"
  [ch tr]
  (dosync (alter (:traits ch) conj tr)
          ((:add tr) (:model ch))))

(defn remove-trait
  "Remove a trait from a character"
  [ch tr]
  (dosync (alter (:traits ch) disj tr)
          ((:remove tr) (:model ch))))

(defmacro basic-trait
  "Create a basic trait factory.  cost is a function returning a
   single cell, cell-builder is a function returning a list of cells."
  [trait-name cost-builder cell-builder]
  `(struct-map trait-factory
     :name ~trait-name
     :make (fn []
             (let [cost# (~cost-builder)
                   cells# (conj (~cell-builder) cost#)]
               (struct-map trait
                 :name ~trait-name
                 :modifiables [(make-modifiable mod# [1 1] 1)]
                 :cost cost#
                 :add (fn [ch#]
                        (add-cells ch# cells#))
                 :remove (fn [ch#]
                           (remove-cells ch# cells#)))))))

(defmacro variable-trait
  "A trait that can vary according to a source"
  [trait-name modifiable-builder cost-builder cell-builder]
  `(struct-map trait-factory
     :name ~trait-name
     :make (fn []
             (let [mod# (~modifiable-builder)
                   cost# (~cost-builder)
                   cells# (conj (~cell-builder) cost#)]
               (struct-map trait
                 :name ~trait-name
                 :modifiables [mod#]
                 :cost cost#
                 :add (fn [ch#]
                        (add-modifiable ch# mod#)
                        (add-cells ch# cells#))
                 :remove (fn [ch#]
                           (remove-cells ch# cells#)
                           (remove-modifiable ch# mod#)))))))


;;; Secondary Traits

(def secondary-stat-cost-table
     (make-table 8 [1 2 2 2 3 5 7 8 9 10 11 12 13]))

(defn secondary-cost
  "Compute the standard secondary cost"
  [level primary mult]
  (* mult (apply + (for [i (range 1 (inc level))]
                     (let [val (+ primary (* i mult))]
                       (secondary-stat-cost-table val))))))

(defmacro secondary-validators
  "Create the secondary validation cells"
  [prim cell-name]
  (let [val-name (symbol (str (name prim) "-secondary-mod"))]
    `(list (cell ~'secondary-stat-modifier (quote ~cell-name))
           (cell ~val-name (quote ~cell-name)))))

(defmacro standard-secondary-trait
  "A trait-factory to build a standard secondary trait modifier.
   Direction is :increase or :decrease.  If display-name is nil, it
   will equal the cell-name"
  [cell-name secondary primary direction]
  (let [display-name (make-display-name cell-name)
        multiplier (cond
                    (= direction :increase) 1
                    (= direction :decrease) -1
                    :otherwise (throwf Exception "Bad direction %s" (str :direction)))
        modifier-name (symbol (str (name secondary) "-mods"))]
    `(struct-map trait-factory
         :name ~display-name
         :make (fn []
                 (let [main-cell# (cell :source ~cell-name 1)
                       modifiable# (make-modifiable ~cell-name [1 2] 1)
                       cost-cell# (cell ~'cp-cost
                                        (secondary-cost ~(var-from-name cell-name)
                                                        ~(var-from-name primary)
                                                        ~multiplier))
                       mod-cell# (cell ~modifier-name (* ~(var-from-name cell-name)
                                                         ~multiplier))
                       [val1-cell# val2-cell#] (secondary-validators
                                                ~primary
                                                ~cell-name)
                       cells# [cost-cell# mod-cell# val1-cell# val2-cell#]]
                   (struct-map trait
                     :name ~display-name
                     :modifiables [modifiable#]
                     :cost cost-cell#
                     :add (fn [char#]
                            (add-modifiable char# modifiable#)
                            (add-cells char# cells#))
                     :remove (fn [char#]
                               (remove-cells char# cells#)
                               (remove-modifiable char# modifiable#))))))))
       
(defmacro basic-secondary-trait
  "Create a basic secondary trait.  Add cells, is a function returning
   additional cells."
  [trait-name cost secondary primary add-cells]
  (let [val1-name (symbol (str (name primary) "-secondary-mod"))]
    `(basic-trait ~trait-name
                  (fn [] (cell ~'cp-cost ~cost))
                  (fn []
                    (let [[val1# val2#] (secondary-validators ~primary ~trait-name)
                          add-cells# (~add-cells)]
                      (list* val1# val2# add-cells#))))))

;;; Skills

(def expensive-skill-cost
     (make-table 8 [1/4 1/2 1 2 3 4 5 6 12 21 29 37 45]))
(def expensive-skill-linked-cost
     (make-table -3 [1/2 1 2 3 4 6 11 14 19 23]))
(def standard-skill-cost
     (make-table 10 [1/4 1/2 1 2 3 4 5 13 21 27 35]))
(def standard-skill-linked-cost
     (make-table -2 [1/4 1/2 1 2 3 4 6 14 23 31]))
(def level-cost-expensive
     (make-table 1 [-1 0 4 16]))
(def level-cost-standard
     (make-table 1 [-1 0 2 12]))

(defn lookup-linked-cost
  [table roll stat]
  (let [diff (- roll stat)]
    (try (table diff)
         (catch IndexOutOfBoundsException e (. java.lang.Integer MAX_VALUE)))))

(defn skill-cost
  [roll level type stats]
  (let [[t lt l] (cond
                  (= type :standard) [standard-skill-cost
                                      standard-skill-linked-cost
                                      level-cost-standard]
                  (= type :expensive) [expensive-skill-cost
                                       expensive-skill-linked-cost
                                       level-cost-expensive]
                  :default (throwf Exception "Bad skill type %s" type))]
    (+ (l level) (apply min (t roll) (map (partial lookup-linked-cost lt roll)
                                          stats)))))

(defmacro skill
  ([n type stats] `(skill ~n ~type ~stats (fn [] nil)))
  ([n type stats add-cells-builder]
     (let [roll-name (symbol (str (name n) "-roll"))
           level-name (symbol (str (name n) "-level"))]
       `(struct-map trait-factory
          :name ~(make-display-name n)
          :make (fn []
                  (let [roll# (make-modifiable ~roll-name [8 20] 12)
                        level# (make-modifiable ~level-name [1 4] 2)
                        cost# (cell ~'cp-cost (skill-cost ~(var-from-name roll-name)
                                                          ~(var-from-name level-name)
                                                          ~type
                                                          [~@(map #(var-from-name %)
                                                                  stats)]))
                        ac# (~add-cells-builder)]
                    (struct-map trait
                      :name ~(make-display-name n)
                      :modifiables [roll# level#]
                      :cost cost#
                      :add (fn [ch#]
                             (do (add-modifiable ch# roll#)
                                 (add-modifiable ch# level#)
                                 (add-cells ch# (cons cost# ac#))))
                      :remove (fn [ch#]
                                (remove-cells ch# (cons cost# ac#))
                                (remove-modifiable ch# roll#)
                                (remove-modifiable ch# level#)))))))))

(defn compute-grapple-level
  [n roll]
  (if (< n 0)
    (+ roll n)
    n))

(defmacro grapple-bonus
  [which array skill]
  (let [roll (var-from-name (symbol (str (name skill) "-roll")))
        level (var-from-name (symbol (str (name skill) "-level")))
        cell-name (symbol (str (name which) "-grapple-skill-mods"))]
    `(cell ~cell-name (if (>= ~roll 12)
                        [(compute-grapple-level (~array (dec ~level)) ~roll) ~level]
                        [0 0]))))


;;; Trait

(defmacro cost-only-trait
  ([trait-name cost] `(cost-only-trait ~trait-name ~cost (fn [] nil)))
  ([trait-name cost cell-builder]
     `(basic-trait ~(make-display-name trait-name)
                   (fn [] (cell ~'cp-cost ~cost))
                   ~cell-builder)))

(defmacro standard-trait
  ([trait-name cost-vec] `(standard-trait ~trait-name ~cost-vec (fn [] nil)))
  ([trait-name cost-vec cell-builder]
     (let [cost-name (symbol (str (name trait-name) "-cost"))]
       `(variable-trait ~(make-display-name trait-name)
                        (fn [] (make-modifiable ~trait-name [1 ~(count cost-vec)] 1))
                        (fn [] (cell ~'cp-cost
                                     (~cost-vec (dec ~(var-from-name trait-name)))))
                        ~cell-builder))))

;;;;;;
(comment

(def fred (build-character))
(print-dataflow (:model fred))

(def fighting ((:make (skill fighting :expensive [agi]))))
(add-trait fred fighting)
(remove-trait fred fighting)

(def powerful ((:make (standard-secondary-trait powerful str phy :increase))))
(add-trait fred powerful)
(remove-trait fred powerful)

(def puny ((:make (basic-secondary-trait "Puny" -5 str phy
                                         (fn []
                                           [(cell str-mods (- 8 ?phy))])))))

(add-trait fred puny)
(remove-trait fred puny)

(update-values (:model fred) {'fighting-roll 15})
(update-values (:model fred) {'phy 15})

(use :reload 'jagsrpg.model)
(use 'clojure.contrib.stacktrace) (e)
(use 'clojure.contrib.trace)
)

;; End of file
