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
                      JLabel
                      JTextField
                      JButton
                      SwingUtilities)
        '(java.awt.event ActionListener))

(defn tied-label
  "Build a swing label that tracks a stat"
  [ch stat]
  (let [cell (get-cell (:model ch) stat)
        value (-> cell :value deref)
        label (JLabel. (str value))]
    (do
      (add-cell-watcher
        cell
        :key
        (fn [key cell old-v new-v]
          (SwingUtilities/invokeLater
           (fn []
             (println (str old-v))
             (println (.getText label))
             (let [n-s (str new-v)]
               (when (not= n-s (.getText label))
                 (.setText label n-s)))))))
      label)))
        
(def character (build-character))

(def label (tied-label character 'str))
(def frame (JFrame. "Jags Character"))

(doto frame
  (.add label)
  (.setSize 800 600)
;  (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
  (.pack)
  (.setVisible true)
  (.show))
           
(update-values (:model character) {'phy 11})
(print-dataflow (:model character))



;; End of file
