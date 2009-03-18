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
  "Add +/- y to x, or +/- y * 10%, whichever is greater"
  [x y]
  (cond
   (> y 0) (max (+ x y) (round (+ x (* x (/ y 10)))))
   (< y 0) (min (+ x y) (round (+ x (* x (/ y 10)))))
   :otherwise x))

(defn j-mult
  "Multiple x and y and round results"
  [x y]
  (round (* x y)))

(defmacro impact-chart
  "Return two collections of cells representing damage.  Base name is
   the name of the 13-14 cell.  The first collection are those to the
   left of the main score, the second those to the right."
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
                            0)))]
    `[[(cell ~a (if (> ~bn 0)
                  1
                  0))
       ~(left b a 'j-mult 0.1)
       ~(left c b 'j-mult 0.25)
       ~(left d c 'j-mult 0.33)
       ~(left e d 'j-mult 0.5)
       ~(left f e 'j-add -3)
       ~(left g f 'j-add -2)
       ~(left h g 'j-add -1)]
      [(cell ~u (j-add ~bn 1))
       (cell ~v (j-add ~bn 2))
       (cell ~w (j-add ~bn 3))
       (cell ~x (max ~(var-from-name w) (j-mult ~bn 1.5)))
       (cell ~y (j-mult ~bn 1.75))
       (cell ~z (j-mult ~bn 2))]]))


(comment
  (def ch (impact-chart fred))
  (doseq [gr ch
          cl gr]
    (println cl))


  (macroexpand '(impact-chart fred))
  
  (use :reload 'jagsrpg.damage)
  (use 'clojure.contrib.stacktrace) (e)
)
;; End of file
