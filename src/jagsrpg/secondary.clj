;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  secondary.clj
;;
;;  Secondary Trait Modifiers
;;
;;  straszheimjeffrey (gmail)
;;  Created 15 March 2009

(ns jagsrpg.secondary
  (:use jagsrpg.model)
  (:use jls.dataflow.dataflow))

(def secondary-traits
     [(standard-secondary-trait powerful      str phy :increase)
      (standard-secondary-trait weaker        str phy :decrease)
      (standard-secondary-trait big-tall      base-bld phy :increase)
      (standard-secondary-trait slight        base-bld phy :decrease)
      (standard-secondary-trait resiliant     con phy :increase)
      (standard-secondary-trait sickly        con phy :decrease)
      (standard-secondary-trait coordinated   cor ref :increase)
      (standard-secondary-trait klutz         cor ref :decrease)
      (standard-secondary-trait quick         rea ref :increase)
      (standard-secondary-trait slow          rea ref :decrease)
      (standard-secondary-trait agile         agi ref :increase)
      (standard-secondary-trait clumsy        agi ref :decrease)
      (standard-secondary-trait smart         res int :increase)
      (standard-secondary-trait dull-simple   res int :decrease)
      (standard-secondary-trait photographic-memory mem int :increase)
      (standard-secondary-trait forgetfull    res int  :decrease)
      (standard-secondary-trait strong-willed wil int :increase)
      (standard-secondary-trait weak-willed   wil int :decrease)

      (basic-secondary-trait "Puny" -5 str phy
                             (fn []
                               [(cell str-mods (- 8 ?phy))]))
      (basic-secondary-trait "Emaciated" -5 bld phy
                             (fn []
                               [(cell bld-mods (- 7 ?phy))
                                (cell damage-point-mods -4)]))
      (basic-secondary-trait "Fragile" -7 con phy
                             (fn []
                               [(cell con-mods (- 7 ?phy))]))
      (basic-secondary-trait "Crippled Hands" -6 cor ref
                             (fn []
                               [(cell cor-mods (- 7 ?ref))]))
      (basic-secondary-trait "Sluggish" -7 rea ref
                             (fn []
                               [(cell rea-mods (- 7 ?ref))]))
      (basic-secondary-trait "Uncoordinated" -8 agi ref
                             (fn []
                               [(cell agi-mods (- 7 ?ref))]))
      (basic-secondary-trait "Clueless" -4 res int
                             (fn [] nil))
      (basic-secondary-trait "Retarded" -5 res int
                             (fn []
                               [(cell res-mods (- 7 ?int))]))
      (basic-secondary-trait "Empty Headed" -7 mem int
                             (fn []
                               [(cell mem-mods (- 7 ?int))]))
      (basic-secondary-trait "Wishy Washy" -4 wil int
                             (fn [] []))
      (basic-secondary-trait "Weak Minded" -5 wil int
                             (fn []
                               [(cell wil-mods (- 7 ?int))]))

      (variable-trait "Light/Short"
                      (fn [] (make-modifiable light-short [1 3] 1))
                      (fn [] (cell 'cp-cost 0))
                      (fn [] [(cell displayed-bld-mods (* -1 ?light-short))]))
      (variable-trait "Fat/Obese"
                   (fn [] (make-modifiable fat-obese [1 2] 1))
                   (fn [] (cell cp-cost ([-3 -5] (dec ?fat-obese))))
                   (fn [] [(cell bld-mod ([3 8] (dec ?fat-obese)))
                           (cell damage-points-mod ([1 3] (dec ?fat-obese)))
                           (cell agi-mod ([0 -2] (dec ?fat-obese)))]))
      (variable-trait "Ill"
                      (fn [] (make-modifiable ill [1 3] 1))
                      (fn [] (cell cp-cost ([-1 -3 -5] (dec ?ill))))
                      (fn [] []))])

(comment

 (variable-trait "Light/Short"
                 (fn [] (make-modifiable light-short [1 3] 1))
                 (fn [] (cell 'cp-cost 0))
                 (fn [] [(cell displayed-bld-mods (* -1 ?light-short))]))
 
(use :reload 'jagsrpg.secondary)
(use :reload 'jagsrpg.model)
(use 'clojure.contrib.stacktrace) (e)
)

;; End of file

