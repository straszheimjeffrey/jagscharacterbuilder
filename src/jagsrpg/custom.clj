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
  (:use jagsrpg.utilities)
  (:use jagsrpg.model)
  (:use clojure.contrib.dataflow)
  (:use [clojure.contrib.seq-utils :only (find-first)]))

(def custom-mods
     ['ap-cost 'cp-cost :break
      'str 'bld 'con :break
      'cor 'rea 'agi :break
      'res 'mem 'wil :break
      
      'base-damage 'hand-to-hand-damage :break
      'charm 'initmidate 'persuade 'recruit :break
 
      'damage-points :break

      'offensive-grapple 'defensive-grapple :break

      'walking-ground-speed 'running-ground-speed 'sprinting-ground-speed :break

      'initiative 'perception :break

      'agi-bonus 'agi-bonus-hth 'agi-bonus-ranged :break

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
                                                [`(make-modifiable ~scn
                                                                   [-99 99] 
                                                                   0)
                                                 `(cell ~mn ~(var-from-name scn))]))]
                                        (vec (map step (remove keyword? custom-mods))))
                   source-mods# (map first cell-pairs#)
                   mod-cells# (map second cell-pairs#)
                   name-cell# (cell :source ~(symcat n "-name") "-- custom trait --")
                   notes# (cell :source ~(symcat n "-notes") "")
                   all-cells# (conj mod-cells# name-cell# notes#)]
               (struct-map trait
                 :name ~(make-display-name n)
                 :symb-name (quote ~n)
                 :type :custom
                 :modifiables source-mods#
                 :cost nil ; Handled by the main cell matrix
                 :notes notes#
                 :cells (concat (mapcat get-modifiable-cells source-mods#)
                                all-cells#))))))

(defmacro make-custom-traits
  []
  (vec (for [n (range 12)]
         `(custom-trait ~(symcat "custom-trait-" n)))))

(def custom-traits (make-custom-traits))

(defn- get-custom-trait
  "Scans to find a custom trait not in collection n"
  [n]
  (letfn [(step [w]
                (not-any? #(= (:name w) (:name %)) n))]
      (find-first step custom-traits)))

(defn get-free-custom-trait
  "Gets the next free trait for character"
  [ch]
  (get-custom-trait @(:traits ch)))

(defn get-source-list
  "Returns a list of [display-name, cell-name] for the sources"
  [tr]
  (let [tr-n (:symb-name tr)
        step (fn [n]
               (if (symbol? n)
                 [(make-display-name n) (symcat tr-n "-" n)]
                 n))]
    (map step custom-mods)))

(defn serialize-trait
  "Converts a trait to a serialized representation"
  [tr]
  (let [name (:symb-name tr)
        len (.length (str name))
        cells (filter source-cell? (:cells tr))
        names (map (fn [c] (.substring (-> c :name str) len)) cells)
        vals (map get-value-from-cell cells)]
    (zipmap names vals)))

(defn deserialize-trait
  "Unserialized a trait, then adds it to a character"
  [ch trs]
  (let [tr ((:make (get-free-custom-trait ch)))
        n (:symb-name tr)
        vals (into {} (map (fn [[k v]] [(symcat n k) v]) trs))]
    (dosync (add-traits ch [tr])
            (update-values (:model ch) vals)
            tr)))

(comment

  (def cs ((:make (custom-trait custom-trait-0))))
  (:modifiables cs) (:symb-name cs)
  (def cust (serialize-trait cs))

  (def fred (build-character))
  (print-dataflow (:model fred))
  (deserialize-trait fred cust)

  (use :reload 'jagsrpg.custom)
  (use 'clojure.contrib.stacktrace) (e)
)
          

;; End of file

