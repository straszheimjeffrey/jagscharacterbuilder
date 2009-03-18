;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  traits.clj
;;
;;  Traits
;;
;;  straszheimjeffrey (gmail)
;;  Created 15 March 2009

(ns jagsrpg.traits
  (:use jagsrpg.model)
  (:use clojure.contrib.dataflow)
  (:use [clojure.contrib.math :only (round)]))

(defmacro st-trait
  "Define a standard trait"
  [& args]
  `(trait-base :trait ~@args))

(def standard-traits*
     [(st-trait ambidextrous [12])
      (st-trait asthma [-4])
      (st-trait bad-eyesight [-1 -3]
                [(cell perception-mods ([-1 -4] (dec ?bad-eyesight)))])
      (st-trait beautiful-voice [4]
                [(cell charm-mods 1)
                 (cell persuade-mods 1)])
      (st-trait blind [-15])
      (st-trait conditioning [1 2 4])
      (st-trait contact-ally [1 2 4])
      (st-trait crippled [-2 -4])
      (st-trait deaf [-2 -4])
      (st-trait fast-runner [1 4]
                [(cell rea-for-speed-mods ([2 4] (dec ?fast-runner)))])
      (st-trait hearty [2])
      (st-trait hunchback [-2]
                [(cell agi-mods -1)
                 (cell bld-mods 1)])
      (st-trait iron-jaw [8 16]
                [(cell damage-points-mods 3)
                 (cell con-mods ([0 1] (dec ?iron-jaw)))])
      (st-trait nasal-voice [-2]
                [(cell charm-mods -1)
                 (cell persuade-mods -1)])
      (st-trait out-of-shape [-2 -3])
      (st-trait natural-fighter [2]
                [(cell initiative-mods 2)])
      (st-trait one-eyed [-1 -2])
      (st-trait peg-legged [-1 -4])
      (variable-trait "Toughness"
                      :trait
                      (make-modifiable toughness [1 9999] 1)
                      (cell cp-cost (let [t ?toughness]
                                      (+ t
                                         (if (> t 4)
                                           (- t 4)
                                           0))))
                      [(cell damage-points-mods ?toughness)])
      (st-trait ugly [-2 -4]
                [(cell charm-mods ([-2 -6] (dec ?ugly)))
                 (cell persuade-mods ([-1 -3] (dec ?ugly)))
                 (cell recruit-mods ([-1 -3] (dec ?ugly)))])
      (st-trait visible-mark [-1])
      (st-trait young [-1])
      (st-trait addictive [-2 -4])
      (st-trait artistic-genius [2 4]
                [(cell mem-for-art-mods ([2 4] (dec ?artistic-genius)))])
      (st-trait bad-judgement [-2])
      (st-trait dsiturbed [-2 -4])
      (st-trait flair [1 2 4]
                [(cell charm-mods 1)])
      (st-trait hard-to-fool [2 4 8])
      (st-trait leader [2 4 8 12]
                [(cell persuade-mods ?leader)
                 (cell recruit-mods (* 2 ?leader))])
      (st-trait likable [2 4 8 12]
                [(cell charm-mods (round (* 3/2 ?likable)))
                 (cell persuade-mods (round (* 3/2 ?likable)))])
      (st-trait mathematical-genius [4])
      (st-trait musical-genius [4])
      (st-trait perceptive [2 4 8]
                [(cell perception-mods ([1 2 4] (dec ?perceptive)))])
      (st-trait phobic [-1 -2])
      (st-trait presence [2 4 8 12]
                [(cell intimidate-mods (* 2 ?presence))
                 (cell recruit-mods ?presence)])
      (st-trait sense-of-direction [1 2 4])
      (st-trait speed-reader [4])
      (st-trait baaad-reputation [1 2 4 8]
                [(cell charm-mods (round (/ ?baaad-reputation -2)))
                 (cell intimidate-mods ?baaad-reputation)
                 (cell recruit-mods ?baaad-reputation)])
      (st-trait bad-reputation [-1 -2 -3 -4]
                [(cell charm-mods (* -1 ?bad-reputation))
                 (cell intimidate-mods (round (/ ?bad-reputation 2)))
                 (cell persuade-mod (* -1 ?bad-reputation))
                 (cell recruit-mods (round (/ ?bad-reputation -2)))])
      (st-trait enemy [-1 -3 -5])
      (st-trait good-reputation [2 4 8 12]
                [(cell charm-mods ?good-reputation)
                 (cell persuade-mods (round (/ ?good-reputation 2)))
                 (cell recruit-mods (round (/ ?good-reputation 2)))])
      (st-trait relationship [1 4])
      (st-trait sense-of-duty [2 -2 -4])
      (st-trait special-status [-2 1 2])
      (st-trait ward [-2 -4])
      (st-trait wealth [1 2 4 8 12])
      (st-trait write-up [-2])
      (st-trait attractive [2 4 8 12]
                [(cell charm-mods (round (* 3/2 ?attractive)))
                 (cell persuade-mods (round (* 3/2 ?attractive)))])
      (st-trait bad-temper [-2])
      (st-trait big-mouth [-2])
      (st-trait coward [-4])
      (st-trait curious [-2])
      (st-trait envious [-1 -2])
      (st-trait exotic [2 4 8 12]
                [(cell charm-mods (* 2 ?exotic))
                 (cell persuade-mods (round (* 3/2 ?exotic)))])
      (st-trait greed [-2])
      (st-trait grim [-1])
      (st-trait honorable [-2])
      (st-trait lousy-liar [-3])
      (st-trait lousy-driver [-2])
      (st-trait lustful [-2])
      (st-trait naive [-2])
      (st-trait obnoxious [-1 -2 -3])
      (st-trait cute [1]
                [(cell charm-mods 1)
                 (cell intimidate-mods -2)
                 (cell recruit-mods -1)])
      (st-trait serious [1]
                [(cell charm-mods -1)
                 (cell intimidate-mods -1)
                 (cell recruit-mods 1)])
      (st-trait studious [1]
                [(cell charm-mods -1)
                 (cell intimidate-mods -1)
                 (cell persuade-mods 1)])
      (st-trait scary [1]
                [(cell charm-mods -2)
                 (cell intimidate-mods 1)
                 (cell persuade-mods -1)])
      (st-trait fun [1]
                [(cell charm-mods 1)
                 (cell intimidate-mods -2)
                 (cell recruit-mods -1)])
      (st-trait loud [1]
                [(cell charm-mods -1)
                 (cell intimidate-mods 1)
                 (cell persuade-mods -1)])
      (st-trait pride [-2])
      (st-trait problem-with-authority [-4])
      (st-trait rugged [3]
                [(cell intimidate-mods 2)
                 (cell recruit-mods 2)])
      (st-trait slick [-2])
      (st-trait sloth [-2])
      (st-trait statuesque [2 4 8 12]
                [(cell charm-mods ?statuesque)
                 (cell intimidate-mods ?statuesque)
                 (cell persuade-mods ?statuesque)
                 (cell recruit-mods ?statuesque)])
      (st-trait swashbuckler [-2])])
      
(def standard-traits (sort-by :name standard-traits*))

(comment
  (use :reload 'jagsrpg.traits)
)

;; End of file
