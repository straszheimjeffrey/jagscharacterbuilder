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
  (:use jls.dataflow.dataflow))

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
      (stnadard-trait peg-legged [-1 -4])
      (variable-trait "Toughness"
                      (fn [] (make-modifiable toughness [1 9999] 1))
                      (fn [] (cell toughness-cost (let [t ?toughness]
                                                    (+ t
                                                       (if (> t 4)
                                                         (- t 4)
                                                         0)))))
                      (fn [] [(cell damage-point-mods ?toughness)]))
      (standard-trait ugly [-2 -4])
      (cost-only-trait visible-mark -1)
      (cost-only-trait young -1)
      (standard-trait addictive [-2 -4])
      (standard-trait artistic-genius [2 4]
                      (fn [] [(cell mem-for-art-mods ([2 4] (dec ?artistic-genius)))]))
      (cost-only-trait bad-judgement -2)
      (standard-trait dsiturbed [-2 -4])
      


;; End of file
