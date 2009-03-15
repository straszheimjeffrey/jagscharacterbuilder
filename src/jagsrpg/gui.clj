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
  (:use clojure.contrib.miglayout))

(import '(javax.swing JFrame
                      JPanel
                      JSpinner
                      SpinnerNumberModel
                      JLabel
                      JButton
                      SwingUtilities)
        '(javax.swing.event ChangeListener)
        '(java.awt FlowLayout))

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
                                  (update-values (:model ch)
                                                 {(-> modifiable :cell :name)
                                                  cur-gui}))))))
    spinner))

(def character (build-character))

(def phy (first (filter #(= (-> % :cell :name) 'phy) (:primary-stats character))))

(def label (tied-label character 'str))
(def spinner (tied-spinner character phy))
(def frame (JFrame. "Jags Character"))

(doto frame
  (.setLayout (new FlowLayout))
  (.add spinner)
  (.add label)
  (.setSize 800 600)
;  (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
  (.pack)
  (.setVisible true)
  (.show))
           
;(update-values (:model character) {'phy 11})
(print-dataflow (:model character))

(comment
  (use :reload 'jagsrpg.gui)
)

;; End of file
