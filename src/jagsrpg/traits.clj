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
  (:use clojure.contrib.dataflow))


(def standard-traits
     [(cost-only-trait ambidextrous 12)
      (cost-only-trait asthma -4)
      (standard-trait bad-eyesight [-1 -3]
                      (fn [] [(cell perception-mods ([-1 -4] (dec ?bad-eyesight)))]))
      (cost-only-trait beautiful-voice 4)
      (cost-only-trait blind -15)
      (standard-trait conditioning [1 2 4])
      (standard-trait contact-ally [1 2 4])
      (standard-trait cpippled [-2 -4])
      (standard-trait deaf [-2 -4])
      (standard-trait fast-runner [1 4]
                      (fn [] [(cell rea-for-speed-mods ([2 4] (dec ?fast-runner)))]))
      (cost-only-trait hearty 2)
      (cost-only-trait hunchback
                       (fn [] [(cell agi-mods -1)
                               (cell bld-mods 1)]))
      (standard-trait iron-jaw [8 16]
                      (fn [] [(cell damage-points-mods 3)
                              (cell con-mods ([0 1] (dec ?iron-jaw)))]))
      (cost-only-trait nasal-voice -2)
      (standard-trait out-of-shape [-2 -3])
      (cost-only-trait natural-fighter 2
                       (fn [] [(cell initiative-mods 2)]))
      (standard-trait one-eyed [-1 -2])
      (standard-trait peg-legged [-1 -4])
      (variable-trait "Toughness"
                      (fn [] (make-modifiable toughness [1 9999] 1))
                      (fn [] (cell cp-cost (let [t ?toughness]
                                                    (+ t
                                                       (if (> t 4)
                                                         (- t 4)
                                                         0)))))
                      (fn [] [(cell damage-points-mods ?toughness)]))
      (standard-trait ugly [-2 -4])
      (cost-only-trait visible-mark -1)
      (cost-only-trait young -1)
      (standard-trait addictive [-2 -4])
      (standard-trait artistic-genius [2 4]
                      (fn [] [(cell mem-for-art-mods ([2 4] (dec ?artistic-genius)))]))
      (cost-only-trait bad-judgement -2)
      (standard-trait dsiturbed [-2 -4])
      (standard-trait flair [1 2 4])
      (standard-trait hard-to-fool [2 4 8])
      (standard-trait leader [2 4 8 12])
      (standard-trait likable [2 4 8 12])
      (cost-only-trait mathematical-genius 4)
      (cost-only-trait musical-genius 4)
      (standard-trait perceptive [2 4 8]
                      (fn [] [(cell perception-mods ([1 2 4] (dec ?perceptive)))]))
      (standard-trait phobic [-1 -2])
      (standard-trait presence [2 4 8 12])
      (standard-trait sense-of-direction [1 2 4])
      (cost-only-trait speed-reader 4)
      (standard-trait baaad-reputation [1 2 4 8])
      (standard-trait bad-reputation [-1 -2 -3 -4])
      (standard-trait enemy [-1 -3 -5])
      (standard-trait good-reputation [2 4 8 12])
      (standard-trait relationship [1 4])
      (standard-trait sense-of-duty [2 -2 -4])
      (standard-trait special-status [-2 1 2])
      (standard-trait ward [-2 -4])
      (standard-trait wealth [1 2 4 8 12])
      (cost-only-trait write-up -2)
      (standard-trait attractive [2 4 8 12])
      (cost-only-trait bad-temper -2)
      (cost-only-trait big-mouth -2)
      (cost-only-trait coward -4)
      (cost-only-trait curious -2)
      (standard-trait envious [-1 -2])
      (standard-trait exotic [2 4 8 12])
      (cost-only-trait greed -2)
      (cost-only-trait grim -1)
      (cost-only-trait honorable -2)
      (cost-only-trait lousy-liar -3)
      (cost-only-trait lousy-driver -2)
      (cost-only-trait lustful -2)
      (cost-only-trait naive -2)
      (standard-trait obnoxious [-1 -2 -3])
      (cost-only-trait cute 1)
      (cost-only-trait serious 1)
      (cost-only-trait scary 1)
      (cost-only-trait fun 1)
      (cost-only-trait loud 1)
      (cost-only-trait pride -2)
      (cost-only-trait problem-with-authority -4)
      (cost-only-trait rugged 3)
      (cost-only-trait slick -2)
      (cost-only-trait sloth -2)
      (standard-trait statuesque [2 4 8 12])
      (cost-only-trait swashbuckler -2)])
      


;; End of file
