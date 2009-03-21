;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  html.clj
;;
;;  Creating an HTML Version of a Character
;;
;;  straszheimjeffrey (gmail)
;;  Created 20 March 2009

(ns jagsrpg.html
  (:use jagsrpg.model)
  (:use jagsrpg.damage)
  (:use jagsrpg.secondary)
  (:use jagsrpg.skills)
  (:use jagsrpg.traits)
  (:use jagsrpg.archetype)
  (:use jagsrpg.custom)
  (:use jagsrpg.serialize)
  (:use clojure.contrib.dataflow)
  (:use [clojure.contrib.walk :only (postwalk)])
  (:use [clojure.contrib.str-utils :only (str-join)]))
;  (:use [clojure.contrib.seq-utils :only (find-first)])
;  (:use [clojure.contrib.duck-streams :only (writer)]))


;;; Some useful macros

(defmacro stat
  [n]
  (let [n (symbol (.substring (name n) 1))]
    `(get-value (:model ~'ch) '~n)))

(defn- make-attrs
  [m]
  (if m
    (apply str
           (for [[k v] m]
             (str " " (name k) "=\"" v "\"")))
    ""))

(defmacro html*
  [tag & body]
  (let [[attrs body] (if (map? (first body))
                       [(first body) (next body)]
                       [nil body])
        step (fn [el]
               (cond
                (list? el) (list* 'html* el)
                (and (symbol? el)
                     (= \? (-> el name first))) (list 'stat el)
                     :otherwise el))
        body (map step body)]
    `(str "<"
          '~tag
          ~(make-attrs attrs)
          ~@(if body
              `(">" ~@body "</" '~tag ">")
              [" />"]))))

(defmacro html
  [tags]
  (list* 'html* tags))

(defn- td
  [txt]
  (html (td txt)))

(defn- th
  [txt]
  (html (th txt)))


;;; The panels

(defn- top-panel
  [ch]
  (html (table {:id "name-and-costs"}
               (tr (th "Name") (td ?name)
                   (th "CP Cost") (td ?total-cp-cost)
                   (th "AP Cost") (td ?total-ap-cost)))))

(defn- stat-panel
  [ch]
  (html (table {:id "main-stat"}
               (tr (th "PHY") (td ?phy)
                   (td ?phy-cost)
                   (th "STR") (td ?str)
                   (th "BLD") (td ?displayed-bld)
                   (th "CON") (td ?con))
               (tr (th "REF") (td ?ref)
                   (td ?ref-cost)
                   (th "COR") (td ?cor)
                   (th "REA") (td ?rea)
                   (th "AGI") (td ?agi))
               (tr (th "INT") (td ?int)
                   (td ?int-cost)
                   (th "RES") (td ?res)
                   (th "MEM") (td ?mem)
                   (th "WIL") (td ?wil)))))

(defn- derived-panel
  [ch]
  (html (table {:id "derived-stat"}
               (tr (th "DP") (td ?damage-points)
                   (th "Charm") (td ?charm))
               (tr (th "Perception") (td ?perception)
                   (th "Intimidate") (td ?intimidate))
               (tr (th "Initiative") (td ?initiative)
                   (th "Persuade") (td ?persuade))
               (tr (th "Speed") (td ?walking-ground-speed "/"
                                    ?running-ground-speed "/"
                                    ?sprinting-ground-speed)
                   (th "Recruit") (td ?recruit))
               (tr (th "Base Damage") (td ?base-damage))
               (tr (th "Grapple") (td ?offensive-grapple "/"
                                      ?defensive-grapple)
                   (th "Armor") (td "dr " ?armor-dr " pen " ?armor-pen)))))

(defn- damage-panel
  [ch]
  (html (table {:id "damage"}
               (tr (th {:rowspan 4} "Normal") (td {:rowspan 4} "0")
                   (th "Subminor") (td 1))
               (tr (th "Minor") (td ?minor-wound-level))
               (tr (th "Major") (td ?major-wound-level))
               (tr (th "Critical") (td ?critical-wound-level))
               (tr (th {:rowspan 3} "Hurt") (td {:rowspan 3} ?hurt-condition)
                   (th "Minor") (td "1"))
               (tr (th "Major") (td ?major-wound-level))
               (tr (th "Critical") (td ?critical-wound-level))
               (tr (th {:rowspan 2} "Injured") (td {:rowspan 2} ?injured-condition)
                   (th "Major") (td ?minor-wound-level))
               (tr (th "Critical") (td ?major-wound-level))
               (tr (th {:rowspan 2} "Serious") (td {:rowspan 2} ?serious-condition)
                   (th "Major") (td "1"))
               (tr (th "Critical") (td ?minor-wound-level)))))

(defn- trait-row
  [tr]
  (let [n (:name tr)
        val (-> tr :modifiables first :cell get-value-from-cell)
        cost (-> tr :cost get-value-from-cell)]
    (html (tr (th n) (td val) (td cost)))))

(defn- skill-row
  [sk]
  (let [n (:name sk)
        roll (-> sk :modifiables first :cell get-value-from-cell)
        level (-> sk :modifiables second :cell get-value-from-cell)
        cost (-> sk :cost get-value-from-cell)]
    (html (tr (th n) (td roll) (td level) (td cost)))))

(defn- traits-panel
  [ch]
  (let [trs (filter #(or (= :secondary %)
                         (= :trait %)) @(:traits ch))
        tr-html (apply str (map trait-row trs))]
    (html (table {:id "traits"} tr-html))))

(defn- a-traits-panel
  [ch]
  (let [trs (filter #(= :archetype %) @(:traits ch))
        tr-html (apply str (map trait-row trs))]
    (html (table {:id "a-traits"} tr-html))))

(defn- skills-panel
  [ch]
  (let [sks (filter #(= :skill %) @(:traits ch))
        tr-html (apply str (map skill-row sks))]
    (html (table {:id "skills"} tr-html))))

(defn- damage-row
  [ch name symbs]
  (let [td (fn [s]
             (let [v (get-value (:model ch) s)]
               (html (td v))))
        vals (apply str (map td symbs))]
    (html (tr (th name) vals))))

(defn- hth-skill-rows
  [ch sk]
  (let [sn (:bare-name sk)
        pn (punch-name sn)
        cn (cross-name sn)
        kn (kick-name sn)
        p-symbs (get-impact-symbols pn)
        c-symbs (get-impact-symbols cn)
        k-symbs (get-impact-symbols kn)
        p-html (damage-row ch (make-display-name pn) p-symbs)
        c-html (damage-row ch (make-display-name cn) c-symbs)
        k-html (damage-row ch (make-display-name kn) k-symbs)]
    (str p-html c-html k-html)))

(defn- weapon-row
  [ch wpn]
  (let [name (get-value (:model ch) (symcat (:main-name wpn) "-name"))
        symbs (:symbols wpn)
        html (damage-row ch name symbs)]
    html))

(defn- impact-damage-panel
  [ch]
  (let [hths (filter #(= :hth (:hth %)) @(:traits ch))
        i-wpns (filter #(= :impact-weapon (:type %)) @(:traits ch))
        s-rs (apply str (map (partial hth-skill-rows ch) hths))
        w-rs (apply str (map (partial weapon-row ch) i-wpns))
        header-ns (cons "Name" impact-names)
        header (apply str (map th header-ns))]
    (html (table {:id "impact-damage"}
                 (tr header)
                 s-rs
                 w-rs))))
        
(defn- penetrating-damage-panel
  [ch]
  (let [i-wpns (filter #(= :penetrating-weapon (:type %)) @(:traits ch))
        w-rs (apply str (map (partial weapon-row ch) i-wpns))
        header-ns (cons "Name" penetrating-names)
        header (apply str (map th header-ns))]
    (html (table {:id "penetrating-damage"}
                 (tr header)
                 w-rs))))

(defn- custom-row
  [ch tr]
  (let [sn (:symb-name tr)
        n (get-value (:model ch) (symcat sn "-name"))
        ccn (symcat sn "-cp-cost")
        acn (symcat sn "-ap-cost")
        cc (get-value (:model ch) ccn)
        ac (get-value (:model ch) acn)]
    (html (tr (th n) (td "AP Cost") (td ac) (td "CP Cost") (td cc)))))

(defn- custom-panel
  [ch]
  (let [ctrs (filter #(= :custom (:type %)) @(:traits ch))
        rws (apply str (map (partial custom-row ch) ctrs))]
    (html (table {:id "custom-traits"} rws))))

(defn html-page
  [ch]
  (let [name (-> ch :model (get-value 'name))
        top (top-panel ch)
        stat (stat-panel ch)
        derived (derived-panel ch)
        damage (damage-panel ch)
        trait (traits-panel ch)
        a-trait (a-traits-panel ch)
        skill (skills-panel ch)
        i-dam (impact-damage-panel ch)
        p-dam (penetrating-damage-panel ch)
        custom (custom-panel ch)]
    (html (html (head (title name))
                (body top 
                      stat
                      derived
                      damage
                      trait
                      a-trait
                      skill
                      i-dam
                      p-dam
                      custom)))))
        


(comment

  (let [joe "joe"]
    (html (html {:fred "mary"} joe)))

  (macroexpand '(stat phy))

 ; (def character (build-character))
  (def character jagsrpg.gui/character)
  (print-dataflow (:model character))

  (html-page character)

  (trait-row ((:make (first secondary-traits))))

  (use :reload 'jagsrpg.html)
  (use 'clojure.contrib.stacktrace) (e)
)

;; End of file
