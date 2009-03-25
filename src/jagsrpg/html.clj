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
  (:use jagsrpg.utilities)
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
             (let [nm (if (string? k)
                        k
                        (name k))]
               (str " " nm "=\"" v "\""))))
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

(defn- th-wpn
  [txt]
  (html (th {:class "damage-header"} txt)))

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
        cost (-> tr :cost get-value-from-cell)
        notes (-> tr :notes get-value-from-cell)]
    (html (tr (th n) (td val) (td cost) (td notes)))))

(defn- skill-row
  [sk]
  (let [n (:name sk)
        roll (-> sk :modifiables first :cell get-value-from-cell)
        level (-> sk :modifiables second :cell get-value-from-cell)
        cost (-> sk :cost get-value-from-cell)
        notes (-> sk :notes get-value-from-cell)]
    (html (tr (th n) (td roll) (td level) (td cost) (td notes)))))

(defn- traits-rows
  [ch]
  (let [trs (sort-by :name (filter #(or (= :secondary (:type %))
                                        (= :trait (:type %))) @(:traits ch)))]
        (apply str (map trait-row trs))))

(defn- a-traits-rows
  [ch]
  (let [trs (sort-by :name (filter #(= :archetype (:type %)) @(:traits ch)))]
    (apply str (map trait-row trs))))

(defn- skills-rows
  [ch]
  (let [sks (sort-by :name (filter #(= :skill (:type %)) @(:traits ch)))]
    (apply str (map skill-row sks))))

(defn- damage-row
  [ch name symbs]
  (let [td (fn [s]
             (let [v (get-value (:model ch) s)]
               (html (td v))))
        vals (apply str (map td symbs))]
    (html (tr (th name) vals))))

(defn- hth-skill-rows
  [ch sk]
  (let [sn (:symb-name sk)
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
  (let [name (get-value (:model ch) (symcat (:symb-name wpn) "-name"))
        symbs (:symbols wpn)
        html (damage-row ch name symbs)]
    html))

(defn- impact-damage-panel
  [ch]
  (let [hths (sort-by :name (filter #(= :hth (:hth %)) @(:traits ch)))
        i-wpns (sort-by :name (filter #(= :impact-weapon (:type %)) @(:traits ch)))
        s-rs (apply str (map (partial hth-skill-rows ch) hths))
        w-rs (apply str (map (partial weapon-row ch) i-wpns))
        header-ns (cons "Name" impact-names)
        header (apply str (map th-wpn header-ns))]
    (html (table {:id "impact-damage"}
                 (tr header)
                 s-rs
                 w-rs))))
        
(defn- penetrating-damage-panel
  [ch]
  (let [i-wpns (sort-by :name (filter #(= :penetrating-weapon (:type %)) @(:traits ch)))
        w-rs (apply str (map (partial weapon-row ch) i-wpns))
        header-ns (cons "Name" penetrating-names)
        header (apply str (map th-wpn header-ns))]
    (html (table {:id "penetrating-damage"}
                 (tr header)
                 w-rs))))

(defn- weapon-notes-row
  [ch wpn]
  (let [name (get-value (:model ch) (symcat (:symb-name wpn) "-name"))
        notes (-> wpn :notes get-value-from-cell)]
    (if (and notes (> (.length notes) 0))
      (html (tr (td name) (td notes)))
      "")))

(defn- weapons-notes-table
  [ch]
  (let [wpns (sort-by :name (filter #(or (= :impact-weapon (:type %))
                                         (= :penetrating-weapon (:type %)))
                                    @(:traits ch)))
        rows (apply str (map (partial weapon-notes-row ch) wpns))]
    (if (> (.length rows) 0)
      (html (table (tr (th "Name") (th "Notes"))
                   rows))
      rows)))

(defn- custom-row
  [ch tr]
  (let [sn (:symb-name tr)
        n (get-value (:model ch) (symcat sn "-name"))
        ccn (symcat sn "-cp-cost")
        acn (symcat sn "-ap-cost")
        cc (get-value (:model ch) ccn)
        ac (get-value (:model ch) acn)
        notes (-> tr :notes get-value-from-cell)]
    (html (tr (th n) (td "&nbsp;") (td ac "/" cc) (td notes)))))

(defn- custom-rows
  [ch]
  (let [ctrs (sort-by :name (filter #(= :custom (:type %)) @(:traits ch)))]
    (apply str (map (partial custom-row ch) ctrs))))

(defn styles
  [& body]
  (let [pairs (partition 2 body)
        each (fn [[selector styles]]
               (let [each (fn [[k v]]
                            (str (if (string? k) k (name k)) ": " v ";\n"))
                     flat (apply str (map each styles))]
                 (str selector " {\n" flat "}\n")))]
    (apply str (map each pairs))))
        
(def style
     (let [styles (styles "body" {:font-family "Verdana, Geneva, sans-serif"
                                  :font-size "small"}
                          "td, th" {:vertical-align "top"
                                    :padding "2px 4px"}
                          "td" {:text-align "center"}
                          "th" {:text-align "left"}
                          "th.damage-header" {:text-align "center"
                                              :min-width "0.125in"})]
       (html (style {:type "text/css"}
                    "<!--\n"
                    styles
                    "-->"))))

(defn html-page
  [ch]
  (let [name (-> ch :model (get-value 'name))
        top (top-panel ch)
        stat (stat-panel ch)
        derived (derived-panel ch)
        damage (damage-panel ch)
        trait (traits-rows ch)
        a-trait (a-traits-rows ch)
        custom (custom-rows ch)
        skill (skills-rows ch)
        i-dam (impact-damage-panel ch)
        p-dam (penetrating-damage-panel ch)
        wpn-notes (weapons-notes-table ch)]
    (dosync
     (str "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" "
          "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"> "
          (html (html {:xmlns "http://www.w3.org/1999/xhtml"
                       :lang "en"
                       "xml:lang" "en"}
                      (head (meta {:http-equiv "Content-Type"
                                   :content "text/html; charset=UTF-8"})
                            (title name)
                            style)
                      (body (div {:id "top-content"}
                                 (table (tr {:colspan "2"}
                                            (td top))
                                        (tr (td stat)
                                            (td {:rowspan "2"}
                                                damage))
                                        (tr (td derived))))
                            (div {:id "bottom-content"}
                                 (table (tr
                                         (td
                                          (table {:id "skills-table"}
                                                 (tr (th "Name")
                                                     (th "Roll")
                                                     (th "Level")
                                                     (th "Cost")
                                                     (th "Notes"))
                                                 skill))
                                         (td
                                          (table {:id "traits"}
                                                 (tr (th "Name")
                                                     (th "Level")
                                                     (th "Cost")
                                                     (th "Notes"))
                                                 trait
                                                 (tr (th {:colspan "4"}))
                                                 a-trait
                                                 (tr (th {:colspan "4"}))
                                                 custom))))
                                 i-dam
                                 p-dam
                                 wpn-notes))))))))
                                            
                                 

(comment
  (use :reload 'jagsrpg.html)
  (use 'clojure.contrib.stacktrace) (e)
)

;; End of file
