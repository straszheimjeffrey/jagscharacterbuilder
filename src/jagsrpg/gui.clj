;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  gui.clj
;;
;;  The GUI Layout
;;
;;  straszheimjeffrey (gmail)
;;  Created 15 March 2009

(ns jagsrpg.gui
  (:use jagsrpg.model)
  (:use jagsrpg.damage)
  (:use jagsrpg.secondary)
  (:use jagsrpg.skills)
  (:use jagsrpg.traits)
  (:use jagsrpg.archetype)
  (:use jagsrpg.custom)
  (:use jagsrpg.serialize)
  (:use jagsrpg.html)
  (:use clojure.contrib.dataflow)
  (:use [clojure.contrib.seq-utils :only (find-first)])
  (:use [clojure.contrib.duck-streams :only (writer)]))

(import '(java.io File
                  FileReader
                  PushbackReader)
        '(javax.swing JFrame
                      JPanel
                      JSpinner
                      SpinnerNumberModel
                      JLabel
                      JList
                      JTextField
                      DefaultListModel
                      ListSelectionModel
                      JButton
                      JMenuBar
                      JMenu
                      JMenuItem
                      JScrollPane
                      JTabbedPane
                      JFileChooser
                      JSplitPane
                      JDialog
                      SwingUtilities)
        '(javax.swing.filechooser FileFilter)
        '(java.awt.event ActionListener
                         WindowAdapter)
        '(javax.swing.event ChangeListener
                            DocumentListener)
        '(java.awt FlowLayout)
        '(net.miginfocom.swing MigLayout))

(def frame-count (atom 0))

(defn label
  [t]
  (JLabel. t))

(defn scroll
  [panel]
  (let [sp (JScrollPane. panel)]
    sp))

(defn v-split
  [p1 p2]
  (let [sp (JSplitPane. JSplitPane/VERTICAL_SPLIT
                        p1
                        p2)]
    (doto sp
      (.setResizeWeight 0.5))))

; The following crap just should not be necessary!
(defn validate-to-top
  [com]
  (.invalidate com)
  (.repaint com)
  (if (isa? (class com) javax.swing.JFrame)
    (.validate com)
    (.revalidate com))
  (let [p (.getParent com)]
    (when p
      (recur p))))

(defn find-frame
  [com]
  (let [p (.getParent com)]
    (if p
      (recur p)
      p)))


;;; Components tied to model objects

(defn tied-label
  "Build a swing label that tracks a stat"
  ([ch stat] (tied-label (get-cell (:model ch) stat)))
  ([cell]
     (let [value (get-value-from-cell cell)
           label (JLabel. (str value))]
       (do
         (add-cell-watcher
          cell
          (gensym "key")
          (fn [key cell old-v new-v]
            (SwingUtilities/invokeLater
             (fn []
               (let [n-s (str new-v)]
                 (when (not= n-s (.getText label))
                   (.setText label n-s)))))))
         label))))

(defn tied-spinner
  "Build a spinner control that tracks a modifiable"
  [ch modifiable]
  (let [min (-> modifiable :range first)
        max (-> modifiable :range second)
        val (-> modifiable :cell :value deref)
        model (SpinnerNumberModel. val min max 1)
        spinner (JSpinner. model)]
    (when (= min max)
      (.setEnabled spinner false))
    (.addChangeListener
              spinner
              (proxy [ChangeListener] []
                (stateChanged [evt]
                              (let [cur-m (-> modifiable :cell get-value-from-cell)
                                    cur-gui (.getValue spinner)]
                                (when (not= cur-m cur-gui)
                                  (try
                                   (update-values (:model ch)
                                                  {(-> modifiable :cell :name)
                                                   cur-gui})
                                   (catch Exception e
                                     (.setValue spinner cur-m))))))))
    spinner))

(defn tied-text-box
  "Build a text box that updates a cell"
  [ch stat width]
  (let [cell (get-cell (:model ch) stat)
        init (get-value-from-cell cell)
        text-box (JTextField. init width)
        document (.getDocument text-box)
        content-changed (fn []
                          (let [cur-m (get-value-from-cell cell)
                                cur-gui (.getText text-box)]
                            (when (not= cur-m cur-gui)
                              (try
                               (update-values (:model ch)
                                              {(:name cell) cur-gui})
                               (catch Exception e
                                 (SwingUtilities/invokeLater
                                  (fn []
                                    (.setText text-box cur-m))))))))]
    (do
      (.addDocumentListener document
                            (proxy [DocumentListener] []
                              (changeUpdate [evt] (content-changed))
                              (removeUpdate [evt] (content-changed))
                              (insertUpdate [evt] (content-changed))))
      text-box)))

;;; Top panels

(defn cost-panel
  [ch]
  (let [layout (MigLayout.)
        panel (JPanel. layout)]
    (doto panel
      (.add (label "Name"))
      (.add (tied-text-box ch 'name 15))
      (.add (label "CP Cost"))
      (.add (tied-label ch 'total-cp-cost))
      (.add (label "AP Cost"))
      (.add (tied-label ch 'total-ap-cost)))))

(defn main-stat-panel
  [ch]
  (let [layout (MigLayout. "wrap 9")
        panel (JPanel. layout)
        add-row (fn [p-name prim sec1-name sec1 sec2-name sec2 sec3-name sec3]
                  (let [prim-mod (get-primary-stat ch prim)
                        cost-name (symbol (str (name prim) "-cost"))]
                    (doto panel
                      (.add (label p-name))
                      (.add (tied-spinner ch prim-mod))
                      (.add (tied-label ch cost-name))
                      (.add (label sec1-name))
                      (.add (tied-label ch sec1))
                      (.add (label sec2-name))
                      (.add (tied-label ch sec2))
                      (.add (label sec3-name))
                      (.add (tied-label ch sec3)))))]
    (do
      (add-row "PHY" 'phy "STR" 'str "BLD" 'displayed-bld "CON" 'con)
      (add-row "REF" 'ref "COR" 'cor "REA" 'rea           "AGI" 'agi)
      (add-row "INT" 'int "RES" 'res "MEM" 'mem           "WIL" 'wil)
      panel)))

(defn derived-stat-panel
  [ch]
  (let [layout (MigLayout. "wrap 2")
        panel (JPanel. layout)
        left-layout (MigLayout. "wrap 2")
        left-panel (JPanel. left-layout)
        right-layout (MigLayout. "wrap 2")
        right-panel (JPanel. right-layout)]
    (do (doto panel
          (.add left-panel)
          (.add right-panel))
        (doto left-panel
          (.add (label "DP"))
          (.add (tied-label ch 'damage-points))
          (.add (label "Perception"))
          (.add (tied-label ch 'perception))
          (.add (label "Initiative"))
          (.add (tied-label ch 'initiative))
          (.add (label "Speed"))
          (.add (tied-label ch 'walking-ground-speed) "split 3")
          (.add (tied-label ch 'running-ground-speed))
          (.add (tied-label ch 'sprinting-ground-speed))
          (.add (label "Base Damage"))
          (.add (tied-label ch 'base-damage))
          (.add (label "Grapple"))
          (.add (tied-label ch 'offensive-grapple) "split 2")
          (.add (tied-label ch 'defensive-grapple)))
        (doto right-panel
          (.add (label "Charm"))
          (.add (tied-label ch 'charm))
          (.add (label "Intimidate"))
          (.add (tied-label ch 'intimidate))
          (.add (label "Persuade"))
          (.add (tied-label ch 'persuade))
          (.add (label "Recruit"))
          (.add (tied-label ch 'recruit))
          (.add (label "Armor"))
          (.add (tied-label ch 'armor-dr) "split 2")
          (.add (tied-label ch 'armor-pen)))
        panel)))

(defn damage-panel
  [ch]
  (let [layout (MigLayout. "wrap 4" "[][]5%[][]")
        panel (JPanel. layout)]
    (doto panel
      (.add (label "Normal") "sy 4, top")
      (.add (label "0") "sy 4, top")
      
      (.add (label "Sub Minor"))
      (.add (label "1"))
      (.add (label "Minor"))
      (.add (tied-label ch 'minor-wound-level))
      (.add (label "Major"))
      (.add (tied-label ch 'major-wound-level))
      (.add (label "Critical"))
      (.add (tied-label ch 'critical-wound-level))
      
      (.add (label "Hurt") "sy 3, top")
      (.add (tied-label ch 'hurt-condition) "sy 3, top")
      
      (.add (label "Minor"))
      (.add (label "1"))
      (.add (label "Major"))
      (.add (tied-label ch 'major-wound-level))
      (.add (label "Critical"))
      (.add (tied-label ch 'critical-wound-level))
      
      (.add (label "Injured") "sy 2, top")
      (.add (tied-label ch 'injured-condition) "sy 2, top")

      (.add (label "Major"))
      (.add (tied-label ch 'minor-wound-level))
      (.add (label "Critical"))
      (.add (tied-label ch 'major-wound-level))

      (.add (label "Serious") "sy 2, top")
      (.add (tied-label ch 'serious-condition) "sy 2, top")
      
      (.add (label "Major"))
      (.add (label "1"))
      (.add (label "Critical"))
      (.add (tied-label ch 'minor-wound-level)))))

(defn top-panel
  [ch]
  (let [layout (MigLayout. "wrap 2" "[].1in[]" "[].1in[]")
        panel (JPanel. layout)]
    (doto panel
      (.add (cost-panel ch) "sx 2")
      (.add (main-stat-panel ch))
      (.add (damage-panel ch) "sy 2")
      (.add (derived-stat-panel ch)))))


;;; Bottom Panels

;; Custom Trait Dialog

(defn custom-dialog
  [ch fr ct]
  (let [dia (JDialog. fr)
        cp (.getContentPane dia)
        layout (MigLayout.)
        panel (JPanel. layout)
        n (:symb-name ct)
        nm (tied-text-box ch (symcat n "-name") 30)
        sl (get-source-list ct)
        step (fn [mod]
               [(-> mod :cell :name) mod])
        mod-map (into {} (map step (:modifiables ct)))
        cb (JButton. "Close")]
    (do (.setLayout panel layout)
        (.add panel (label "Optional Trait") "sx 5, wrap")
        (.add panel nm "sx 5, wrap")
        (loop [f (first sl)
               n (next sl)
               m (fnext sl)]
          (when f
            (when (-> f keyword? not)
              (do (.add panel (label (first f)) "right")
                  (if (= m :break)
                    (.add panel (tied-spinner ch (mod-map (second f))) "wrap")
                    (.add panel (tied-spinner ch (mod-map (second f)))))))
            (recur (first n) (next n) (fnext n))))
        (.add panel cb "wrap")
        (.addActionListener cb
                 (proxy [ActionListener] []
                   (actionPerformed [evt]
                           (.setVisible dia false))))
        (.add cp (scroll panel))
        dia)))


;; Custom Traits

(defn add-custom-to-gui
  [ch fr dp ct]
  (let [ct-name (:symb-name ct)
        name-label (tied-label ch (symcat ct-name "-name"))
        cp-label (tied-label ch (symcat ct-name "-cp-cost"))
        ap-label (tied-label ch (symcat ct-name "-ap-cost"))
        eb (JButton. "Edit")
        db (JButton. "Delete")
        dia (custom-dialog ch fr ct)]
    (do (.add dp name-label)
        (.add dp ap-label)
        (.add dp cp-label)
        (.add dp eb)
        (.add dp db "wrap")
        (.addActionListener eb
                  (proxy [ActionListener] []
                    (actionPerformed [evt]
                            (.pack dia)
                            (.setVisible dia true))))
        (.addActionListener db
                  (proxy [ActionListener] []
                    (actionPerformed [evt]
                           (remove-trait ch ct)
                           (.remove dp name-label)
                           (.remove dp cp-label)
                           (.remove dp ap-label)
                           (.remove dp eb)
                           (.remove dp db)
                           (validate-to-top dp))))
        (validate-to-top dp))))
    
(defn custom-display-panel
  [ch]
  (let [layout (MigLayout. "" "[].2in[]")
        panel (JPanel. layout)]
    (doto panel
      (.add (label "Name"))
      (.add (label "AP Cost"))
      (.add (label "CP Cost") "wrap"))))

(defn custom-panel
  [ch fr]
  (let [layout (MigLayout. "wrap 1")
        panel (JPanel. layout)
        dp (custom-display-panel ch)
        button (JButton. "Add")
        cts (filter #(= :custom (:type %)) @(:traits ch))]
    (do (.add panel (scroll dp))
        (.add panel button)
        (.addActionListener button
                            (proxy [ActionListener] []
                              (actionPerformed [evt]
                                    (let [nct ((:make (get-custom-trait @(:traits ch))))]
                                      (add-trait ch nct)
                                      (add-custom-to-gui ch fr dp nct)))))
        (doseq [ct cts]
          (add-trait ch ct)
          (add-custom-to-gui ch fr dp ct))
        panel)))


;; Weapons stuff

(defn add-weapon-to-gui
  [ch dp w]
  (let [labels (for [sym (:symbols w)]
                 (if (= sym (:main-name w))
                   (tied-spinner ch (-> w :modifiables first))
                   (tied-label ch sym)))
        n (tied-text-box ch (-> w :name-cell :name) 6)
        d (JButton. "Delete")]
    (do (.add dp n)
        (doseq [lab labels]
          (.add dp lab))
        (.add dp d "wrap")
        (.addActionListener d
                   (proxy [ActionListener] []
                     (actionPerformed [evt]
                            (remove-trait ch w)
                            (.remove dp n)
                            (doseq [lab labels]
                              (.remove dp lab))
                            (.remove dp d)
                            (validate-to-top dp))))
        (validate-to-top dp))))

(defn weapon-display-panel
  [ch names]
  (let [layout (MigLayout. "" "[].1in[.2in:n:n, align 50%]")
        panel (JPanel. layout)
        labels (map label names)]
    (do (.add panel (label "Name"))
        (doseq [label (butlast labels)]
          (.add panel label))
        (.add panel (last labels) "wrap")
        panel)))

(defn weapon-panel
  [ch names weapons t]
  (let [layout (MigLayout. "wrap 1")
        panel (JPanel. layout)
        dp (weapon-display-panel ch names)
        button (JButton. "Add")
        ws (filter #(= t (:type %)) @(:traits ch))]
    (do (.add panel (scroll dp))
        (.add panel button)
        (.addActionListener button
                 (proxy [ActionListener] []
                   (actionPerformed [evt]
                         (let [nw ((:make (get-weapon weapons @(:traits ch))))]
                           (add-trait ch nw)
                           (add-weapon-to-gui ch dp nw)))))
        (doseq [w ws]
          (add-trait ch w)
          (add-weapon-to-gui ch dp w))
        [panel dp])))


;; Trait and skill addition

(defn add-trait-to-gui
  ([ch dp trait] (add-trait-to-gui ch dp trait (fn [] nil) (fn [] nil)))
  ([ch dp trait extra-adds extra-removes]
     (let [nl (label (:name trait))
           sps (map (partial tied-spinner ch) (:modifiables trait))
           c (tied-label (:cost trait))
           d (JButton. "Delete")]
       (add-trait ch trait)
       (.add dp nl)
       (doseq [s sps] (.add dp s))
       (.add dp c)
       (.add dp d "wrap")
       (extra-adds)
       (.addActionListener d
                           (proxy [ActionListener] []
                             (actionPerformed [evt]
                                              (remove-trait ch trait)
                                              (.remove dp nl)
                                              (doseq [s sps] (.remove dp s))
                                              (.remove dp c)
                                              (.remove dp d)
                                              (extra-removes)
                                              (validate-to-top dp))))
       (validate-to-top dp))))

(defn add-hth-skill-to-gui
  [ch dp trait dam-dp]
  (let [base-name (:bare-name trait)
        labels (fn [base]
                 (cons (-> base make-display-name label)
                       (map (partial tied-label ch) (get-impact-symbols base))))
        punch-labels (labels (symcat base-name "-punch"))
        cross-labels (labels (symcat base-name "-cross"))
        kick-labels (labels (symcat base-name "-kick"))
        add-one-set (fn [labels]
                      (do (doseq [lab (butlast labels)]
                            (.add dam-dp lab))
                          (.add dam-dp (last labels) "wrap")))
        remove-one-set (fn [labels]
                         (doseq [lab labels]
                           (.remove dam-dp lab)))
        add (fn [] (do (add-one-set punch-labels)
                       (add-one-set cross-labels)
                       (add-one-set kick-labels)))
        remove (fn [] (do (remove-one-set punch-labels)
                          (remove-one-set cross-labels)
                          (remove-one-set kick-labels)))]
    (add-trait-to-gui ch dp trait add remove)))
  

;; Standard bottom panels

(defn trait-selection-list
  [ch factories dp extra]
  (let [lm (DefaultListModel.)
        list (JList. lm)
        button (JButton. "Add")
        layout (MigLayout. "wrap 1")
        panel (JPanel. layout)]
    (do
      (doseq [f factories]
        (.addElement lm (:name f)))
      (doto list
        (.setSelectionMode (ListSelectionModel/SINGLE_SELECTION)))
      (doto button
        (.addActionListener
         (proxy [ActionListener] []
           (actionPerformed [evt]
                  (let [n (.getSelectedValue list)
                        f (find-first #(= n (:name %)) factories)]
                    (when f
                      (let [tr ((:make f))]
                        (do
                          (if (:hth tr)
                            (add-hth-skill-to-gui ch dp tr extra)
                            (add-trait-to-gui ch dp tr))
                          (.revalidate panel)))))))))
        (doto panel
          (.add (scroll list))
          (.add button)))))
  
(defn trait-display-panel
  [ch]
  (let [layout (MigLayout.)
        panel (JPanel. layout)]
    panel))

(defn trait-panel
  [ch factories tp extra]
  (let [dp (trait-display-panel ch)
        layout (MigLayout. "wrap 2" "[50%:n:n][]")
        panel (JPanel. layout)
        traits (filter #(= tp (:type %)) @(:traits ch))]
    (do (doto panel
          (.add (scroll dp) "grow")
          (.add (trait-selection-list ch factories dp extra)))
        (doseq [t traits]
          (if (:hth t)
            (add-hth-skill-to-gui ch dp t extra)
            (add-trait-to-gui ch dp t)))
        panel)))

(defn skill-and-weapon-panel
  [ch factories tp]
  (let [[ip idp] (weapon-panel ch
                               impact-names
                               impact-weapons
                               :impact-weapon)
        skill-panel (trait-panel ch factories tp idp)]
    [skill-panel ip]))
                     
(defn bottom-panel
  [ch fr]
  (let [[skill-panel weap-panel] (skill-and-weapon-panel ch skills :skill)
        [pp _] (weapon-panel ch
                             penetrating-names
                             penetrating-weapons
                             :penetrating-weapon)]
    (doto (JTabbedPane.)
      (.add "Secondary" (trait-panel ch secondary-traits :secondary nil))
      (.add "Skills" skill-panel)
      (.add "Traits" (trait-panel ch standard-traits :trait nil))
      (.add "Archetypes" (trait-panel ch archetypes :archetype nil))
      (.add "Impact" (scroll weap-panel))
      (.add "Penetrating" pp)
      (.add "Custom" (custom-panel ch fr)))))


;;; Menu Operations

(defn- add-character-to-frame
  [fr ch]
  (let [cp (.getContentPane fr)]
    (do (doto cp
          (.add (top-panel ch))
          (.add (bottom-panel ch fr)))
        (.pack fr))))

(declare show-frame)

(def file-filter (proxy [FileFilter] []
                  (accept [f]
                          (.matches (.getName f) ".*\\.jags$"))
                  (getDescription [] "JAGS Characters")))

(def html-file-filter (proxy [FileFilter] []
                       (accept [f]
                               (.matches (.getName f) ".*\\.html$"))
                       (getDescription [] "HTML Document")))

(defn new-character
  [fr ch]
  (show-frame (build-character)))

(defn open-character
  [fr ch old]
  (let [chooser (JFileChooser.)]
    (do
      (.setFileFilter chooser file-filter)
      (let [result (.showOpenDialog chooser fr)]
        (when (= result JFileChooser/APPROVE_OPTION)
          (with-open [r (PushbackReader. (FileReader. (.getSelectedFile chooser)))]
            (let [n (read r)
                  nch (deserialize-character n)]
                                        ; (if (= old (serialize-character ch))
                                        ; (do (add-character-to-frame fr nch)
                                        ;     (.pack fr))
                (show-frame nch))))))))
              
(defn save-character
  [fr ch]
  (let [chooser (JFileChooser.)
        cm (-> ch :model (get-cell 'name) :value deref)]
    (.setFileFilter chooser file-filter)
    (.setSelectedFile chooser (File. (str cm ".jags")))
    (let [result (.showSaveDialog chooser fr)
          fix-name (fn [n]
                     (if (.accept file-filter n)
                       n
                       (File. (str (.getPath n) ".jags"))))]
      (when (= result JFileChooser/APPROVE_OPTION)
        (with-open [w (writer (fix-name (.getSelectedFile chooser)))]
          (binding [*out* w]
            (pr (serialize-character ch))))))))

(defn save-character-as-html
  [fr ch]
  (let [chooser (JFileChooser.)
        cm (-> ch :model (get-cell 'name) :value deref)]
    (.setFileFilter chooser html-file-filter)
    (.setSelectedFile chooser (File. (str cm ".html")))
    (let [result (.showSaveDialog chooser fr)
          fix-name (fn [n]
                     (if (.accept html-file-filter n)
                       n
                       (File. (str (.getPath n) ".html"))))]
      (when (= result JFileChooser/APPROVE_OPTION)
        (with-open [w (writer (fix-name (.getSelectedFile chooser)))]
          (binding [*out* w]
            (pr (html-page ch))))))))

(defn add-menu-bar
  [fr ch]
  (let [bar (JMenuBar.)
        file (JMenu. "File")
        menu-new (JMenuItem. "New")
        menu-open (JMenuItem. "Open")
        menu-save (JMenuItem. "Save")
        menu-html (JMenuItem. "as HTML")
        old (serialize-character ch)]
    (do
      (.addActionListener menu-new
                 (proxy [ActionListener] []
                   (actionPerformed [evt]
                            (new-character fr ch))))
      (.addActionListener menu-open
                 (proxy [ActionListener] []
                   (actionPerformed [evt]
                            (open-character fr ch old))))
      (.addActionListener menu-save
                 (proxy [ActionListener] []
                   (actionPerformed [evt]
                            (save-character fr ch))))
      (.addActionListener menu-html
                 (proxy [ActionListener] []
                   (actionPerformed [evt]
                            (save-character-as-html fr ch))))
      (.add bar file)
      (.add file menu-new)
      (.add file menu-open)
      (.add file menu-save)
      (.add file menu-html)
      (.setJMenuBar fr bar))))


;;; Frame

(defn show-frame
  [ch]
  (swap! frame-count inc)
  (let [layout (MigLayout. "fill, wrap 1" "" "[].2in[growprio 200]")
        frame (JFrame. "Jags Character")]
    (doto frame
      (.setLayout layout)
      (add-menu-bar ch)
      (add-character-to-frame ch)
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.addWindowListener (proxy [WindowAdapter] []
                            (windowClosed [evt]
                                          (when (= 0 (swap! frame-count dec))
                                            (. java.lang.System (exit 0))))))
      (.setVisible true)
      (.show))))
           

(comment

  (def character (build-character))
  (swap! frame-count inc)
  @frame-count

  (show-frame character)
  (print-dataflow (:model character))
  (:traits character)
  (def ser1 (serialize-character character))
  (def character (deserialize-character ser1))
  
  (use :reload 'jagsrpg.gui)
  (use :reload 'jagsrpg.model)
  (use 'clojure.contrib.stacktrace) (e)
)

;; End of file
