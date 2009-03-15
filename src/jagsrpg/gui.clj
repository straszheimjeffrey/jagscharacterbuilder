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
  (:use jagsrpg.secondary)
  (:use jls.dataflow.dataflow)
  (:use [clojure.contrib.seq-utils :only (seek)]))

(import '(javax.swing JFrame
                      JPanel
                      JSpinner
                      SpinnerNumberModel
                      JLabel
                      JList
                      DefaultListModel
                      ListSelectionModel
                      JButton
                      JScrollPane
                      SwingUtilities)
        '(java.awt.event ActionListener)
        '(javax.swing.event ChangeListener)
        '(java.awt FlowLayout)
        '(net.miginfocom.swing MigLayout))


(defn label
  [t]
  (JLabel. t))



(defn tied-label
  "Build a swing label that tracks a stat"
  ([ch stat] (tied-label (get-cell (:model ch) stat)))
  ([cell]
     (let [value (get-value-from-cell cell)
           label (JLabel. (str value))]
       (do
         (add-cell-watcher
          cell
          :key
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
  (println "m" modifiable)
  (let [min (-> modifiable :range first)
        max (-> modifiable :range second)
        val (-> modifiable :cell :value deref)
        model (SpinnerNumberModel. val min max 1)
        spinner (JSpinner. model)]
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
                                     (do (.setValue spinner cur-m)
                                         (.printStackTrace e)))))))))
    spinner))

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

(defn top-panel
  [ch]
  (let [layout (MigLayout. "wrap 1")
        panel (JPanel. layout)]
    (doto panel
      (.add (main-stat-panel ch))
      (.add (derived-stat-panel ch)))))

(defn scroll
  [panel]
  (let [sp (JScrollPane. panel)]
    sp))

(defn trait-display-panel
  [ch]
  (let [layout (MigLayout. "wrap 4")
        panel (JPanel. layout)]
    panel))
  
(defn trait-selection-list
  [ch factories dp]
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
                        f (seek #(= n (:name %)) factories)]
                    (when f
                      (let [trait ((:make f))
                            nl (label (:name trait))
                            sps (map (partial tied-spinner ch) (:modifiables trait))
                            c (tied-label (:cost trait))
                            d (JButton. "Delete")]
                        (do
                          (add-trait ch trait)
                          (.add dp nl)
                          (doseq [s sps] (.add dp s))
                          (.add dp c)
                          (.add dp d)
                          (.addActionListener d
                               (proxy [ActionListener] []
                                 (actionPerformed [evt]
                                     (remove-trait ch trait)
                                     (.remove dp nl)
                                     (doseq [s sps] (.remove dp s))
                                     (.remove dp c)
                                     (.remove dp d))))))))))))
      (doto panel
        (.add (scroll list))
        (.add button)))))
                                         
(defn trait-panel
  [ch factories]
  (let [dp (trait-display-panel ch)
        layout (MigLayout. "wrap 2")
        panel (JPanel. layout)]
    (doto panel
      (.add (scroll dp))
      (.add (trait-selection-list ch factories dp)))))




(def character (build-character))

(defn show-frame []
  (let [layout (MigLayout. "wrap 1")
        frame (JFrame. "Jags Character")]
    (doto frame
      (.setLayout layout)
      (.add (top-panel character))
      (.add (trait-panel character secondary-traits))
      (.setSize 800 600)
      (.pack)
      (.setVisible true)
      (.show))))
           

(comment
  (show-frame)
  (print-dataflow (:model character))
  
  (use :reload 'jagsrpg.gui)
  (use 'clojure.contrib.stacktrace) (e)
)

;; End of file
