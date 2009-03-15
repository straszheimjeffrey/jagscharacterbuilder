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
  (:use jls.dataflow.dataflow))

(import '(javax.swing JFrame
                      JPanel
                      JSpinner
                      SpinnerNumberModel
                      JLabel
                      JButton
                      SwingUtilities)
        '(javax.swing.event ChangeListener)
        '(java.awt FlowLayout)
        '(net.miginfocom.swing MigLayout))


(defn tied-label
  "Build a swing label that tracks a stat"
  [ch stat]
  (let [cell (get-cell (:model ch) stat)
        value (get-value-from-cell cell)
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
      label)))

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

(defn main-stat-frame
  [ch]
  (let [layout (MigLayout. "wrap 9")
        panel (JPanel. layout)
        add-row (fn [p-name prim sec1-name sec1 sec2-name sec2 sec3-name sec3]
                  (let [prim-mod (get-primary-stat ch prim)
                        cost-name (symbol (str (name prim) "-cost"))]
                    (doto panel
                      (.add (JLabel. p-name) "")
                      (.add (tied-spinner ch prim-mod) "")
                      (.add (tied-label ch cost-name) "")
                      (.add (JLabel. sec1-name) "")
                      (.add (tied-label ch sec1) "")
                      (.add (JLabel. sec2-name) "")
                      (.add (tied-label ch sec2) "")
                      (.add (JLabel. sec3-name) "")
                      (.add (tied-label ch sec3) ""))))]
    (do
      (add-row "PHY" 'phy "STR" 'str "BLD" 'displayed-bld "CON" 'con)
      (add-row "REF" 'ref "COR" 'cor "REA" 'rea           "AGI" 'agi)
      (add-row "INT" 'int "RES" 'res "MEM" 'mem           "WIL" 'wil)
      panel)))
  

(def character (build-character))

(defn show-frame []
  (let [frame (JFrame. "Jags Character")]
    (doto frame
      (.setLayout (new FlowLayout))
      (.add (main-stat-frame character))
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
