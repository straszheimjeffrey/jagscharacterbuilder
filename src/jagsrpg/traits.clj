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
                      (fn []
                        [(cell perception-mods ([-1 -4] (dec ?bad-eyesight)))]))
      (cost-only-trait beautiful-voice 4)
      (cost-only-trait blind -15)
      

;; End of file
