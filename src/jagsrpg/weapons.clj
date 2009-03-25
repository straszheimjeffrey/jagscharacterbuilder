;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  weapons.clj
;;
;;  Weapons
;;
;;  straszheimjeffrey (gmail)
;;  Created 17 March 2009

(ns jagsrpg.weapons
  (:use jagsrpg.utilities)
  (:use jagsrpg.model)
  (:use jagsrpg.damage)
  (:use clojure.contrib.dataflow)
  (:use [clojure.contrib.math :only (round)])
  (:use [clojure.contrib.seq-utils :only (find-first)])
  (:use [clojure.contrib.except :only (throwf)]))


(defmacro make-weapon
  [t n]
  (let [chart (vec (condp = t
                            'impact (impact-chart n)
                            'penetrating (penetrating-chart n)))]
    `(struct-map trait-factory
       :name ~(make-display-name n)
       :make (fn []
               (let [mod# (make-modifiable ~n [-999 999] 0)
                     wn# (cell :source ~(symcat n "-name") "")
                     notes# (cell :source ~(symcat n "-notes") "")
                     cells# (conj ~chart wn# notes#)]
                 (struct-map trait
                   :name ~(make-display-name n)
                   :symb-name (quote ~n)
                   :type ~(keyword (str t "-weapon"))
                   :modifiables [mod#]
                   :cost nil
                   :notes notes#
                   :cells (concat (get-modifiable-cells mod#)
                                  cells#)
                   :name-cell wn#
                   :symbols (quote ~(condp = t
                                             'impact
                                                (get-impact-symbols n)
                                             'penetrating
                                                (get-penetrating-symbols n)))))))))

(defmacro make-weapons
  [t]
  (vec
   (for [n (range 20)]
     (let [n (symcat "wpn-" t "-" n)]
       `(make-weapon ~t ~n)))))
  
(def impact-weapons (make-weapons impact))
(def penetrating-weapons (make-weapons penetrating))

(defn get-weapon
  "Scans l to find a weapon not in trait-factory collection n"
  [l n]
  (let [step (fn [w]
               (not-any? #(= (:name w) (:name %)) n))]
    (find-first step l)))

;; End of file
