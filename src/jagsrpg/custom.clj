;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  custom.clj
;;
;;  Custom Traits
;;
;;  straszheimjeffrey (gmail)
;;  Created 20 March 2009

(ns jagsrpg.custom
  (:use jagsrpg.model)
  (:use clojure.contrib.dataflow)
  (:use [clojure.contrib.seq-utils :only (find-first)]))
;  (:use [clojure.contrib.math :only (round)])
;  (:use [clojure.contrib.except :only (throwf)]))

(def custom-mods
     ['ap-cost 'cp-cost :break
      'str 'bld 'con :break
      'cor 'rea 'agi :break
      'res 'mem 'wil :break
      
      'base-damage 'hth-damage :break
      'charm 'initmidate 'persuade 'recruit
 
      'damage-points :break

      'offensive-grapple 'defensive-grapple :break

      'walking-ground-speed 'running-ground-speed 'sprinting-ground-speed :break

      'initiative 'perception :break

      'armor-dr 'armor-pen :break

      'minor-wound-level 'major-wound-level 'critical-wound-level :break
      'hurt-condition 'injured-condition 'serious-condition :break])

(defmacro custom-trait
  [n]
  `(struct-map trait-factory
     :name ~(make-display-name n)
     :make (fn []
             (let [cell-pairs# ~(letfn [(step [cn]
                                              (let [mn (symcat cn "-mods")
                                                    scn (symcat n "-" cn)]
                                                [`(cell :source ~scn 0)
                                                 `(cell ~mn ~(var-from-name scn))]))]
                                        (vec (map step (remove keyword? custom-mods))))
                   source-cells# (map first cell-pairs#)
                   mod-cells# (map second cell-pairs#)
                   name-cell# (cell :source ~(symcat n "-name") "-- custom trait --")
                   all-cells# (conj (concat source-cells# mod-cells#) name-cell#)]
               (struct-map trait
                 :name ~(make-display-name n)
                 :type :custom
                 :modifiables nil
                 :cost nil ; Handled by the main cell matrix
                 :add (fn [ch#]
                        (add-cells ch# all-cells#))
                 :remove (fn [ch#]
                           (remove-cells ch# all-cells#))
                 :symb-name (quote ~n)
                 :source-cells source-cells#)))))

(defmacro make-custom-traits
  []
  (vec (for [n (range 64)]
         `(custom-trait ~(symcat "custom-trait-" n)))))

(def custom-traits (make-custom-traits))

(defn get-custom-trait
  "Scans to find a custom trait not in collection n"
  [n]
  (letfn [(step [w]
                (not-any? #(= (:name w) (:name %)) n))]
      (find-first step custom-traits)))

(defn get-source-list
  "Returns the symbolic names of the source cells of tr"
  [tr]
  (let [tr-n (:symb-name tr)
        step (fn [n]
               (if (symbol? n)
                 [(make-display-name n) (symcat tr-n "-" n)]
                 n))]
    (map step custom-mods)))

(comment

  (def fred ((:make (custom-trait molly))))

  (def ch (build-character))
  (print-dataflow (:model ch))

  (add-trait ch fred)
  (get-source-list fred)

  (macroexpand '(custom-trait molly))

  (use :reload 'jagsrpg.custom)
  (use 'clojure.contrib.stacktrace) (e)
)
          

;; End of file

