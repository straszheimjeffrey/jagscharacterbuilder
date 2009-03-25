;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  utilities.clj
;;
;;  Some utilities
;;
;;  straszheimjeffrey (gmail)
;;  Created 14 March 2009

(ns jagsrpg.utilities
  (:use clojure.contrib.dataflow)
  (:use [clojure.contrib.str-utils :only (re-split str-join)]))

;;; Utilities

(defn symcat
  "String concats the arguments together to form a symbol"
  [& args]
  (symbol (apply str args)))

(defn var-from-name
  "Given a symbol x, return ?x"
  [symb]
  (symcat "?" symb))

(defn col-from-name
  "Given a symbol x, return ?*x"
  [symb]
  (symcat "?*" symb))

(defn make-display-name
  "Converts name such as fat-obese to Fat Obese"
  [name]
  (str-join " " (map #(apply str (Character/toUpperCase (first %)) (next %))
                     (re-split #"-" (str name)))))

(comment
  (use :reload 'jagsrpg.utilities)
)

;; End of file
