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
  (:use jls.dataflow.dataflow))

(def skills
     [(skill acrobatics       :expensive [agi])
      (skill bow              :expensive [cor])
      (skill boxing           :expensive [agi]
             (fn []
               [(cell hurt-condition-mods
                      (condp = ?boxing-level
                               1 0
                               2 (if (>= ?boxing-roll 13) 1 0)
                               3 2
                               4 8))
                (cell damage-points-mods ([0 0 1 4] (dec ?boxing-level)))]))
      (skill fencing          :expensive [agi])
      (skill firearms         :expensive [cor])
      (skill heavy-weapons    :expensive [cor])
      (skill jujitsu          :expensive [agi]
             (fn []
               [(grapple-bonus offensive [1 2 4 -9] jujitsu)
                (grapple-bonus defensive [1 2 4 -9] jujitsu)]))
      (skill karate           :expensive [agi])
      (skill knife-fighting   :expensive [agi])
      (skill kung-fu          :expensive [agi])
      (skill melee-weapons    :expensive [agi]) 
      (skill staff            :expensive [agi])
      (skill streetfighting   :expensive [agi]
             (fn []
               [(grapple-bonus offensive [1 2 3 -9] streetfighting)
                (grapple-bonus defensive [0 1 2 -12] streetfighting)]))
      (skill tae-kwon-do      :expensive [agi])
      (skill tai-chi          :expensive [agi]
             (fn []
               [(grapple-bonus offensive [0 1 2 -10] tai-chi)
                (grapple-bonus defensive [1 2 4 -7] tai-chi)]))
      (skill tangle-weapons   :expensive [cor])
      (skill thrown-knife     :expensive [cor])
      (skill thrown-weapons   :expensive [cor])
      (skill wrestling        :expensive [agi]
             (fn []
               [(grapple-bonus offensive [2 3 5 -8] wrestling)
                (grapple-bonus defensive [2 3 5 -8] wrestling)]))
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
     

;; End of file
