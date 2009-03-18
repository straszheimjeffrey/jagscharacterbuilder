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
  (:use [clojure.contrib.math :only (round)]))
;  (:use [clojure.contrib.except :only (throwf)]))

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

(defn impact-chart
  "Returns a collection of cell defining forms.  Meant to be used from
   a macro, as the forms are unevaluated."
  [base-name]
  (let [bn (var-from-name base-name)
        a (symcat base-name "-0")
        b (symcat base-name "-1")
        c (symcat base-name "-2-3")
        d (symcat base-name "-4-5")
        e (symcat base-name "-6-7")
        f (symcat base-name "-8-9")
        g (symcat base-name "-10-11")
        h (symcat base-name "-12")
        
        u (symcat base-name "-15")
        v (symcat base-name "-16-17")
        w (symcat base-name "-18-20")
        x (symcat base-name "-21-25")
        y (symcat base-name "-26-29")
        z (symcat base-name "-30")
        left (fn [mc lc fn y]
               `(cell ~mc (if (> ~bn 0)
                            (max ~(var-from-name lc) (~fn ~bn ~y))
                            0)))
        right (fn [mc lc y]
                `(cell ~mc (max ~(var-from-name lc) (j-mult ~bn ~y))))]
    `[(cell ~a (if (> ~bn 0)
                 1
                 0))
      ~(left b a 'j-mult 0.1)
      ~(left c b 'j-mult 0.25)
      ~(left d c 'j-mult 0.33)
      ~(left e d 'j-mult 0.5)
      ~(left f e 'j-add -3)
      ~(left g f 'j-add -2)
      ~(left h g 'j-add -1)
      (cell ~u (j-add ~bn 1))
      (cell ~v (j-add ~bn 2))
      (cell ~w (j-add ~bn 3))
      ~(right x w 1.5)
      ~(right y x 1.75)
      ~(right z y 2)]))

(defn get-impact-names
  [base-name]
  [(symcat base-name "-0")
   (symcat base-name "-1")
   (symcat base-name "-2-3")
   (symcat base-name "-4-5")
   (symcat base-name "-6-7")
   (symcat base-name "-8-9")
   (symcat base-name "-10-11")
   (symcat base-name "-12")
   base-name
   (symcat base-name "-15")
   (symcat base-name "-16-17")
   (symcat base-name "-18-20")
   (symcat base-name "-21-25")
   (symcat base-name "-26-29")
   (symcat base-name "-30")])



(comment
  (def ch (impact-chart 'fred))
  (doseq [cl ch]
    (println cl))


  (macroexpand '(impact-chart fred))
  
  (use :reload 'jagsrpg.damage)
  (use 'clojure.contrib.stacktrace) (e)
)
;; End of file
