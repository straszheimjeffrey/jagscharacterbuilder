;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  damage.clj
;;
;;  Damage Computations
;;
;;  straszheimjeffrey (gmail)
;;  Created 17 March 2009

(ns jagsrpg.damage
  (:use jagsrpg.model)
  (:use clojure.contrib.dataflow)
  (:use [clojure.contrib.math :only (round)])
  (:use [clojure.contrib.seq-utils :only (find-first)])
  (:use [clojure.contrib.except :only (throwf)]))

(defn j-add
  "Add +/- y to x, or +/- y * 10%, whichever is greater.  Will never
   return less than zero."
  [x y]
  (max 0
       (cond
        (> y 0) (max (+ x y) (round (+ x (* x (/ y 10)))))
        (< y 0) (min (+ x y) (round (+ x (* x (/ y 10)))))
        :otherwise x)))

(defn j-mult
  "Multiple x and y and round results.  Will never return negative"
  [x y]
  (if (> x 0)
    (round (* x y))
    (if (> y 1) 3 0)))

(defn first-score
  [n bn]
  `(cell ~n (if (> ~bn 0)
              1
              0)))

(defn next-score
  [n y ln bn]
  (let [bn (var-from-name bn)
        cell-def (fn [fun min]
                   (if min
                     `(cell ~n (if (> ~bn 0)
                                 (max ~(var-from-name ln) (~fun ~bn ~y))
                                 0))
                     `(cell ~n (max ~(var-from-name ln) (~fun ~bn ~y)))))]
    (cond
     (integer? y) (cell-def 'j-add (< y 0))
     (float? y) (cell-def 'j-mult (< y 1))
     (= y :start) (first-score n bn)
     (= y :base) nil
     (= y :plus-one) `(cell ~n (j-add ~bn 1))
     :otherwise (throwf Exception "Bad type %s provided" y))))

(defn damage-chart
  "Returns a collection of cell defining forms.  Meant to be used from
   a macro, as the forms are unevaluated."
  [chart base-name]
  (let [step (fn [[ln acc] [n y]]
               (let [n (symcat base-name "-" n)
                     c (next-score n y ln base-name)]
                 [n (conj acc c)]))]
    (remove nil? (second (reduce step [nil []] chart)))))

(defn damage-names
  [chart]
  (map first chart))
 
(defn get-damage-symbols
  [chart base-name]
  (let [left (take-while #(not= (second %) :base) chart)
        right (next (drop-while #(not= (second %) :base) chart))]
    (concat (map (partial symcat base-name "-") (map first left))
            [base-name]
            (map (partial symcat base-name "-") (map first right)))))

(def impact-def
     (partition 2
                ["0"     :start
                 "1"     0.1
                 "2-3"   0.25
                 "4-5"   0.33
                 "6-7"   0.5
                 "8-9"   -3
                 "10-11" -2
                 "12"    -1
                 "13-14" :base
                 "15"    :plus-one
                 "16-17" 2
                 "18-20" 3
                 "21-25" 1.5
                 "26-29" 1.75
                 "-30"   2.0]))

(def penetrating-def
     (partition 2
                ["0"     :start
                 "1"     0.1
                 "2-3"   0.25
                 "4-5"   0.33
                 "6"     0.5
                 "7-8"   -3
                 "9-10"  -2
                 "11"    -1
                 "12-13" :base
                 "14"    :plus-one
                 "15-16" 2
                 "17-18" 3
                 "19-25" 2.0
                 "26-30" 2.5
                 "31-35" 3.0
                 "36-39" 4.0
                 "40"    8.0]))

(def impact-chart (partial damage-chart impact-def))
(def penetrating-chart (partial damage-chart penetrating-def))

(def impact-names (damage-names impact-def))
(def penetrating-names (damage-names penetrating-def))

(def get-impact-symbols (partial get-damage-symbols impact-def))
(def get-penetrating-symbols (partial get-damage-symbols penetrating-def))

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
                     cells# (conj ~chart wn#)]
                 (struct-map trait
                   :name ~(make-display-name n)
                   :type ~(keyword (str t "-weapon"))
                   :modifiables [mod#]
                   :cost nil
                   :add (fn [ch#]
                          (dosync (add-modifiable ch# mod#)
                                  (add-cells ch# cells#)))
                   :remove (fn [ch#]
                             (dosync (remove-cells ch# cells#)
                                     (remove-modifiable ch# mod#)))
                   :name-cell wn#
                   :main-name '~n
                   :symbols (quote ~(condp = t
                                             'impact (get-impact-symbols n)
                                             'penetrating (get-penetrating-symbols n)))))))))

(defmacro make-weapons
  [t]
  (vec
   (for [n (range 64)]
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

(comment
  (def ch (impact-chart 'fred))
  (doseq [cl ch]
    (println cl))

  (first impact-weapons)

  (damage-chart impact-def 'mary)

  (def ch (build-character))

  (def ww ((:make (make-weapon impact mary))))
  (add-trait ch ww)
  (:add ww)
  (macroexpand '(make-weapon impact mary))
 
  
  (use :reload 'jagsrpg.damage)
  (use 'clojure.contrib.stacktrace) (e)
  (use 'clojure.contrib.trace)
)
;; End of file
