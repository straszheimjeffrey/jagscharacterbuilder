;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  serialize.clj
;;
;;  Convert the character model into serializable type
;;
;;  straszheimjeffrey (gmail)
;;  Created 16 March 2009

(ns jagsrpg.serialize
  (:use jagsrpg.model)
  (:use jagsrpg.traits)
  (:use jagsrpg.secondary)
  (:use jagsrpg.skills)
  (:use jagsrpg.archetype)
  (:use jagsrpg.damage)
  (:use jagsrpg.custom)
  (:use clojure.contrib.dataflow)
  (:use [clojure.contrib.seq-utils :only (find-first)]))

(defn serialize-character
  "Given a character, extract the data needed to rebuild it"
  [ch]
  (dosync
   (let [sc (get-source-cells (:model ch))
         val-map (into {} (map (fn [c] [(:name c) @(:value c)]) sc))
         traits (map (fn [t] [(:name t) (:type t)]) @(:traits ch))]
     {:source-cells val-map
      :traits traits})))

(defn- type-to-trait-collection
  [type]
  (condp = type
           :trait standard-traits
           :secondary secondary-traits
           :skill skills
           :archetype archetypes
           :impact-weapon impact-weapons
           :penetrating-weapon penetrating-weapons
           :custom custom-traits))

(defn deserialize-character
  "Given a form built by serialize-character, make a character again"
  [ser]
  (let [ch (build-character)]
    (do (doseq [td (:traits ser)]
          (let [col (type-to-trait-collection (second td))
                factory (find-first #(= (first td) (:name %)) col)
                tr ((:make factory))]
            (add-trait ch tr)))
        (update-values (:model ch) (:source-cells ser))
        ch)))
    

(comment

  (def fred (build-character))
  (def ser (serialize-character fred))
  (deserialize-character ser)
  


  (use :reload 'jagsrpg.serialize)
  (use 'clojure.contrib.stacktrace) (e)
)
  

;; End of file
