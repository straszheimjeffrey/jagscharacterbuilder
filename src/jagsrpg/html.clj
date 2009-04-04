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
  [& tags]
  (let [step (fn [each]
               `(html* ~@each))
        items (map step tags)]
    `(str ~@ items)))

(defn- td
  [txt]
  (html (td txt)))

(defn- th
  [txt]
  (html (th txt)))

(defn- th-wpn
  [txt]
  (html (th {:class "damage-header"} txt)))

(defn- create-striped-rows
  [rows]
  (loop [f (first rows)
         s (second rows)
         r (nnext rows)
         acc ""]
    (if f
      (recur (first r) (second r) (nnext r)
             (str acc (if s
                        (str (html (tr {:class "odd-row"} f))
                             (html (tr {:class "even-row"} s)))
                        (html (tr {:class "odd-row"} f)))))
      acc)))


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
                   (th "Abl. Dam." (td ?ablative-damage)))
               (tr (th "Perception") (td ?perception)
                   (th "Charm") (td ?charm))
               (tr (th "Initiative") (td ?initiative)
                   (th "Intimidate") (td ?intimidate))
               (tr (th "Speed") (td ?walking-ground-speed "/"
                                    ?running-ground-speed "/"
                                    ?sprinting-ground-speed)
                   (th "Persuade") (td ?persuade))
               (tr (th "Base Damage") (td ?base-damage)
                   (th "Recruit") (td ?recruit))
               (tr (th "Grapple") (td ?offensive-grapple "/"
                                      ?defensive-grapple)
                   (th "Armor") (td "dr " ?armor-dr (br) " pen " ?armor-pen))
               (tr (th "To Be Hit") (td ?tbh-hth "/" ?tbh-ranged)
                   (th "Force Field") (td ?force-field)))))

(defn- damage-panel
  [ch]
  (html (table {:id "damage"}
               (tr (th {:rowspan 4} "Normal") (td {:rowspan 4} "0")
                   (th "Subminor") (td 1))
               (tr (th "Minor") (td ?minor-wound-level))
               (tr (th "Major") (td ?major-wound-level))
               (tr (th "Critical") (td ?critical-wound-level))
               (tr {:class "damage-division"}
                   (th {:rowspan 3} "Hurt") (td {:rowspan 3} ?hurt-condition)
                   (th "Minor") (td "1"))
               (tr (th "Major") (td ?major-wound-level))
               (tr (th "Critical") (td ?critical-wound-level))
               (tr {:class "damage-division"}
                   (th {:rowspan 2} "Injured") (td {:rowspan 2} ?injured-condition)
                   (th "Major") (td ?minor-wound-level))
               (tr (th "Critical") (td ?major-wound-level))
               (tr {:class "damage-division"}
                   (th {:rowspan 2} "Serious") (td {:rowspan 2} ?serious-condition)
                   (th "Major") (td "1"))
               (tr (th "Critical") (td ?minor-wound-level)))))

(defn description
  [ch]
  (html (table {:id "description"}
               (tr (th "Eye Color") (td ?eye-color))
               (tr (th "Hair Color") (td ?hair-color))
               (tr (th "Gender") (td ?gender))
               (tr (th "Height") (td ?height)))
        (h2 "Description")
        (p ?description)
        (h2 "Background")
        (p ?background)))

(defn- trait-row
  [tr]
  (let [n (:name tr)
        val (-> tr :modifiables first :cell get-value-from-cell)
        cost (-> tr :cost get-value-from-cell)
        notes (-> tr :notes get-value-from-cell)]
    (html (tr (th n) (td val) (td cost) (td notes)))))

(defn- skill-row
  [ch sk]
  (let [n (:name sk)
        roll (-> sk :modifiables first :cell get-value-from-cell)
        level (-> sk :modifiables second :cell get-value-from-cell)
        cost (-> sk :cost get-value-from-cell)
        block (if (:hth sk)
                (let [name (symcat (:symb-name sk) "-block")
                      v (get-value (:model ch) name)]
                  (str (first v) " " (second v) " " (nth v 2)))
                "&nbsp;")
        notes (-> sk :notes get-value-from-cell)]
    (html (tr (th n) (td roll) (td level) (td cost) (td block) (td notes)))))

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
    (apply str (map (partial skill-row ch) sks))))

(defn- damage-row
  [ch name symbs]
  (let [td (fn [s]
             (let [v (get-value (:model ch) s)]
               (html (td v))))
        vals (apply str (map td symbs))]
    (str (th name) vals)))

(defn- hth-skill-rows
  [ch sk]
  (let [sn (:symb-name sk)
        names [(punch-name sn) (cross-name sn) (kick-name sn)]
        step (fn [name]
               (let [cells (get-cells (:model ch) name)]
                 (when (-> cells empty? not)
                   (damage-row ch
                               (make-display-name name)
                               (get-impact-symbols name)))))]
    (remove nil? (map step names))))

(defn- weapon-row
  [ch wpn]
  (let [name (get-value (:model ch) (symcat (:symb-name wpn) "-name"))
        symbs (:symbols wpn)]
    (damage-row ch name symbs)))

(defn- impact-damage-panel
  [ch]
  (let [hths (sort-by :name (filter #(= :hth (:hth %)) @(:traits ch)))
        i-wpns (sort-by :name (filter #(= :impact-weapon (:type %)) @(:traits ch)))
        pr (damage-row ch "Punch" (get-impact-symbols 'basic-punch))
        cr (damage-row ch "Cross" (get-impact-symbols 'basic-cross))
        kr (damage-row ch "Kick" (get-impact-symbols 'basic-kick))
        s-rs (mapcat (partial hth-skill-rows ch) hths)
        defaults (if (empty? s-rs)
                   [pr cr kr]
                   nil)
        w-rs (map (partial weapon-row ch) i-wpns)
        rows (create-striped-rows (concat defaults s-rs w-rs))
        headers (map th-wpn impact-names)
        header (apply str (th "Name") headers)]
    (html (table {:id "impact-damage"
                  :class "damage-table"}
                 (tr {:class "table-header"} header)
                 rows))))
        
(defn- penetrating-damage-panel
  [ch]
  (let [i-wpns (sort-by :name (filter #(= :penetrating-weapon (:type %)) @(:traits ch)))
        w-rs (create-striped-rows (map (partial weapon-row ch) i-wpns))
        headers (map th-wpn penetrating-names)
        header (apply str (th "Name") headers)]
    (html (table {:id "penetrating-damage"
                  :class "damage-table"}
                 (tr {:class "table-header"} header)
                 w-rs))))

(defn- weapon-notes-row
  [ch wpn]
  (let [name (get-value (:model ch) (symcat (:symb-name wpn) "-name"))
        notes (-> wpn :notes get-value-from-cell)]
    (if (and notes (> (.length notes) 0))
      (html (tr (th name) (td notes)))
      "")))

(defn- weapons-notes-table
  [ch]
  (let [wpns (sort-by :name (filter #(or (= :impact-weapon (:type %))
                                         (= :penetrating-weapon (:type %)))
                                    @(:traits ch)))
        rows (apply str (map (partial weapon-notes-row ch) wpns))]
    (if (> (.length rows) 0)
      (html (table {:id "weapons-notes-table"
                    :class "weapons-table"}
                   (tr {:class "table-header"} (th "Name") (th "Notes"))
                   rows))
      "")))

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
     (let [styles
           (styles "body" {:font-family "Verdana, Geneva, sans-serif"
                           :font-size "small"}
                   "table" {:border-collapse "collapse"}
                   "h2" {:font-size "1.2em"
                         :font-weight "bold"
                         :margin "10px 0px 5px 0px"}
                   "p" {:margin "5px 0px"}
                   "#top-content-table table" {:margin "2px"
                                               :padding "8px"
                                               :background-color "#ddd"}
                   "td, th" {:vertical-align "top"
                             :padding "2px 4px 2px 8px"
                             :text-align "left"}
                   ".damage-table td, .damage-header" {:text-align "center"
                                                       :min-width "0.125in"
                                                       :padding "2px 4px"}
                   "tr.table-header th" {:border-bottom "1px solid black"}
                   "tr.damage-division td, tr.damage-division th"
                                        {:border-top "1px solid black"}
                   "table.weapons-table, table.damage-table" {:margin "5px 0px 5px 0px"}
                   ".odd-row" {:background-color "#ccc"}
                   )]
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
        description (description ch)
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
                                 (table {:id "top-content-table"}
                                        (tr
                                            (td {:colspan "2"} top))
                                        (tr (td stat)
                                            (td {:rowspan "2"}
                                                damage))
                                        (tr (td derived))))
                            (div {:id "bottom-content"}
                                 (table (tr
                                         (td
                                          (table {:id "skills-table"}
                                                 (tr {:class "table-header"}
                                                     (th "Name")
                                                     (th "Roll")
                                                     (th "Level")
                                                     (th "Cost")
                                                     (th "Block")
                                                     (th "Notes"))
                                                 skill))
                                         (td
                                          (table {:id "traits"}
                                                 (tr {:class "table-header"}
                                                     (th "Name")
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
                                 wpn-notes)
                            (div {:id "extra-content"}
                                 description))))))))

                                            
                                 

(comment
  (use :reload 'jagsrpg.html)
  (use 'clojure.contrib.stacktrace) (e)
  (use 'clojure.contrib.trace)
)

;; End of file
