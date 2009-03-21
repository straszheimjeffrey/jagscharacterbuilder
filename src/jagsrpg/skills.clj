;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  skills.clj
;;
;;  Skills
;;
;;  straszheimjeffrey (gmail)
;;  Created 15 March 2009

(ns jagsrpg.skills
  (:use jagsrpg.model)
  (:use jagsrpg.damage)
  (:use clojure.contrib.dataflow)
  (:use [clojure.contrib.except :only (throwf)]))


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

(defmacro skill-abstract
  [n type stats cells hth]
  (let [roll-name (symcat n "-roll")
        level-name (symcat n "-level")]
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
                     ac# (conj ~cells cost#)]
                 (struct-map trait
                   :name ~(make-display-name n)
                   :symb-name (quote ~n)
                   :type :skill
                   :hth ~hth
                   :modifiables [roll# level#]
                   :cost cost#
                   :cells (concat (get-modifiable-cells roll#)
                                  (get-modifiable-cells level#)
                                  ac#)))))))

(defmacro skill
  ([n type stats] `(skill-abstract ~n ~type ~stats nil nil))
  ([n type stats cells] `(skill-abstract ~n ~type ~stats ~cells nil)))

(defn punch-name [n] (symcat n "-punch"))
(defn cross-name [n] (symcat n "-cross"))
(defn kick-name [n] (symcat n "-kick"))

(defn damage-comp
  [arr level roll]
  (let [n (arr (dec level))]
    (if (< n 0)
      (+ n roll)
      n)))

(defmacro hth-skill
  [n punch cross kick cells]
  (let [level (var-from-name (symcat n "-level"))
        roll (var-from-name (symcat n "-roll"))
        punch-dam (punch-name n)
        cross-dam (cross-name n)
        kick-dam (kick-name n)
        punch-cell `(cell ~punch-dam (+ (damage-comp ~punch ~level ~roll)
                                        ~'?hand-to-hand-damage))
        cross-cell `(cell ~cross-dam (j-add (+ (damage-comp ~cross ~level ~roll)
                                               ~'?hand-to-hand-damage)
                                            1))
        kick-cell `(cell ~kick-dam (j-add (+ (damage-comp ~kick ~level ~roll)
                                             ~'?hand-to-hand-damage)
                                          2))
        punch-chart (impact-chart punch-dam)
        cross-chart (impact-chart cross-dam)
        kick-chart (impact-chart kick-dam)
        cells (vec (concat (list* punch-cell cross-cell kick-cell cells)
                           punch-chart
                           cross-chart
                           kick-chart))]
    `(skill-abstract ~n :expensive [~'agi] ~cells :hth)))
 
(defn compute-grapple-level
  [n roll]
  (if (< n 0)
    (+ roll n)
    n))

(defmacro grapple-bonus
  [which array skill]
  (let [roll (var-from-name (symcat skill "-roll"))
        level (var-from-name (symcat skill "-level"))
        cell-name (symcat which "-grapple-skill-mods")]
    `(cell ~cell-name (if (>= ~roll 12)
                        [(compute-grapple-level (~array (dec ~level)) ~roll) ~level]
                        [0 0]))))


(def skills
     [(skill acrobatics       :expensive [agi])
      (skill bow              :expensive [cor])
      (hth-skill boxing
                 [0 1 2 5]
                 [1 3 4 -9]
                 [0 0 0 0]
                 [(cell hurt-condition-mods
                        (condp = ?boxing-level
                                 1 0
                                 2 (if (>= ?boxing-roll 13) 1 0)
                                 3 2
                                 4 8))
                  (cell damage-points-mods ([0 0 1 4] (dec ?boxing-level)))])
      (skill fencing          :expensive [agi])
      (skill firearms         :expensive [cor])
      (skill heavy-weapons    :expensive [cor])
      (hth-skill jujitsu
                 [0 0 1 2]
                 [0 0 1 2]
                 [0 0 1 2]
                 [(grapple-bonus offensive [1 2 4 -9] jujitsu)
                  (grapple-bonus defensive [1 2 4 -9] jujitsu)])
      (hth-skill karate
                 [1 2 3 -10]
                 [1 2 3 -10]
                 [1 2 3 -10]
                 nil)
      (skill knife-fighting   :expensive [agi])
      (hth-skill kung-fu
                 [0 0 1 2]
                 [0 0 1 2]
                 [0 0 1 2]
                 nil)
      (skill melee-weapons    :expensive [agi]) 
      (skill staff            :expensive [agi])
      (hth-skill streetfighting
                 [0 0 2 3]
                 [0 0 2 3]
                 [0 0 2 3]
                 [(grapple-bonus offensive [1 2 3 -9] streetfighting)
                  (grapple-bonus defensive [0 1 2 -12] streetfighting)])
      (hth-skill tae-kwon-do
                 [0 1 2 4]
                 [0 1 2 4]
                 [0 1 2 -9]
                 nil)
      (hth-skill tai-chi
                 [0 0 0 0]
                 [0 0 0 0]
                 [0 0 0 0]
                 [(grapple-bonus offensive [0 1 2 -10] tai-chi)
                  (grapple-bonus defensive [1 2 4 -7] tai-chi)])
      (skill tangle-weapons   :expensive [cor])
      (skill thrown-knife     :expensive [cor])
      (skill thrown-weapons   :expensive [cor])
      (skill wrestling        :expensive [agi]
             [(grapple-bonus offensive [2 3 5 -8] wrestling)
              (grapple-bonus defensive [2 3 5 -8] wrestling)])
      (skill astrophysics     :expensive [res])
      (skill bio-sciences     :expensive [res])
      (skill chemist          :expensive [res])
      (skill computers        :expensive [res])
      (skill doctor           :expensive [res])
      (skill engineer         :expensive [res])
      (skill mathematics      :expensive [res])
      (skill physics          :expensive [res])
      (skill con-games        :standard  [mem])
      (skill disguise         :standard  [mem])
      (skill escape-artist    :standard  [cor agi mem])
      (skill forgery          :standard  [mem])
      (skill gambling         :standard  [mem])
      (skill hacker           :standard  [res])
      (skill locksmith        :standard  [cor])
      (skill sleight-of-hand  :standard  [cor])
      (skill security-systems :standard  [mem])
      (skill spycraft         :standard  [mem])
      (skill streetwise       :standard  [mem])
      (skill climbing         :standard  [agi])
      (skill demolitions      :standard  [mem])
      (skill stealth          :standard  [agi])
      (skill strategy-tactics :standard  [mem])
      (skill survivalist      :standard  [mem])
      (skill traps            :standard  [cor mem])
      (skill archeology       :standard  [mem])
      (skill art-appreciation :standard  [mem])
      (skill history          :standard  [mem])
      (skill law              :standard  [mem])
      (skill literature       :standard  [mem])
      (skill linguist         :standard  [mem])
      (skill occult-knowlege  :standard  [mem])
      (skill philosophy       :standard  [res])
      (skill phychology       :standard  [res])
      (skill art              :standard  [cor mem-for-art]) ; see artistic-genius
      (skill business-admin   :standard  [res])
      (skill craft-skills     :standard  [cor mem])
      (skill musical-skills   :standard  [mem cor])
      (skill navigation       :standard  [res])
      (skill operations-skill :standard  [mem])
      (skill police-procedure :standard  [mem])
      (skill research         :standard  [mem])
      (skill sports           :standard  [cor agi])
      (skill vehicle-ops      :standard  [cor])
      (skill diplomat         :standard  [res])
      (skill etiquette        :standard  [mem])
      (skill revelry          :standard  [mem])
      (skill showmanship      :standard  [mem con])
      (skill vamp             :standard  [mem con])
      (skill ranged-weapon    :expensive [cor])
      (skill hth-weapon       :expensive [agi])
      (skill general-science  :expensive [res])
      (skill acedemic-disc    :standard  [mem res])
      (skill occupation-disc  :standard  [mem res cor agi])])
     

(comment
  (use :reload 'jagsrpg.skills)
  (use 'clojure.contrib.stacktrace) (e)
)

;; End of file
