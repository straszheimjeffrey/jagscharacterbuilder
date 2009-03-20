;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  secondary.clj
;;
;;  Secondary Trait Modifiers
;;
;;  straszheimjeffrey (gmail)
;;  Created 15 March 2009

(ns jagsrpg.secondary
  (:use jagsrpg.model)
  (:use clojure.contrib.dataflow)
  (:use [clojure.contrib.except :only (throwf)]))

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
  (let [val-name (symcat prim "-secondary-mod")]
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
        modifier-name (symcat secondary "-mods")]
    `(struct-map trait-factory
         :name ~display-name
         :make (fn []
                 (let [modifiable# (make-modifiable ~cell-name [1 2] 1)
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
                     :type :secondary
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
  [trait-name cost secondary primary cells]
  (let [val1-name (symcat primary "-secondary-mod")]
    `(basic-trait ~trait-name
                  :secondary
                  (cell ~'cp-cost ~cost)
                  (let [[val1# val2#] (secondary-validators ~primary ~trait-name)]
                    (list* val1# val2# ~cells)))))

(def secondary-traits
     [(standard-secondary-trait powerful      str phy :increase)
      (standard-secondary-trait weaker        str phy :decrease)
      (basic-secondary-trait "Puny" -5 str phy
                             [(cell str-mods (- 8 ?phy))])

      (standard-secondary-trait big-tall      base-bld phy :increase)
      (standard-secondary-trait slight        base-bld phy :decrease)
      (variable-trait "Light/Short"
                      :secondary
                      (make-modifiable light-short [1 3] 1)
                      (cell cp-cost 0)
                      [(cell displayed-bld-mods (* -1 ?light-short))])
      (basic-secondary-trait "Emaciated" -5 bld phy
                             [(cell bld-mods (- 7 ?phy))
                              (cell damage-point-mods -4)])
      (variable-trait "Fat/Obese"
                      :secondary
                      (make-modifiable fat-obese [1 2] 1)
                      (cell cp-cost ([-3 -5] (dec ?fat-obese)))
                      [(cell bld-mods ([3 8] (dec ?fat-obese)))
                       (cell damage-points-mods ([1 3] (dec ?fat-obese)))
                       (cell agi-mods ([0 -2] (dec ?fat-obese)))
                       (cell charm-mods -2)])

      (standard-secondary-trait resiliant     con phy :increase)
      (standard-secondary-trait sickly        con phy :decrease)
      (basic-secondary-trait "Fragile" -7 con phy
                             [(cell con-mods (- 7 ?phy))])
      (variable-trait "Ill"
                      :secondary
                      (make-modifiable ill [1 3] 1)
                      (cell cp-cost ([-1 -3 -5] (dec ?ill)))
                      nil)

      (standard-secondary-trait coordinated   cor ref :increase)
      (standard-secondary-trait klutz         cor ref :decrease)
      (basic-secondary-trait "Crippled Hands" -6 cor ref
                             [(cell cor-mods (- 7 ?ref))])

      (standard-secondary-trait quick         rea ref :increase)
      (standard-secondary-trait slow          rea ref :decrease)
      (basic-secondary-trait "Sluggish" -7 rea ref
                             [(cell rea-mods (- 7 ?ref))])

      (standard-secondary-trait agile         agi ref :increase)
      (standard-secondary-trait clumsy        agi ref :decrease)
      (basic-secondary-trait "Uncoordinated" -8 agi ref
                             [(cell agi-mods (- 7 ?ref))])

      (standard-secondary-trait smart         res int :increase)
      (standard-secondary-trait dull-simple   res int :decrease)
      (basic-secondary-trait "Clueless" -4 res int nil)
      (basic-secondary-trait "Retarded" -5 res int
                             [(cell res-mods (- 7 ?int))])

      (standard-secondary-trait photographic-memory mem int :increase)
      (standard-secondary-trait forgetfull    res int  :decrease)
      (basic-secondary-trait "Empty Headed" -7 mem int
                             [(cell mem-mods (- 7 ?int))])

      (standard-secondary-trait strong-willed wil int :increase)
      (standard-secondary-trait weak-willed   wil int :decrease)
      (basic-secondary-trait "Wishy Washy" -4 wil int nil)
      (basic-secondary-trait "Weak Minded" -5 wil int
                             [(cell wil-mods (- 7 ?int))])])

(comment


(use :reload 'jagsrpg.secondary)
(use :reload 'jagsrpg.model)
(use 'clojure.contrib.stacktrace) (e)
)

;; End of file

