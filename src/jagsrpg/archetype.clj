;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  archetype.clj
;;
;;  Archetypes
;;
;;  straszheimjeffrey (gmail)
;;  Created 16 March 2009

(ns jagsrpg.archetype
  (:use jagsrpg.model)
  (:use clojure.contrib.dataflow))

(def archetypes*
     [(ar-trait dreamer [2])
      (ar-trait extraordinarily-tough [16]
                [(cell damage-points-mods 8)])
      (ar-trait hard-to-kill [2])
      (ar-trait lucky-miss [4])
      (ar-trait lucky [2 8 16])
      (ar-trait instinct [4])
      (ar-trait malice [4])
      (ar-trait nature-friend [4])
      (ar-trait psychic-link [2])
      (ar-trait see-inner-person [8])
      (ar-trait sensitive [4])
      (ar-trait shadow-friend [8])
      (ar-trait storm-friend [8])
      (ar-trait synchronicity [8])
      (ar-trait twisted-genius [8 12])
      (ar-trait unusual-gear [2 4])
      (ar-trait will-to-fight [4]
                [(cell damage-points-mods 4)])])

(def archetypes (sort-by :name archetypes*))


;; End of file
