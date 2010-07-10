;   Copyright (c) 2010, Erik Soehnel All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns hoeck.pivot.content
  (:use hoeck.pivot.components
        hoeck.pivot.listeners)
  (:import (org.apache.pivot.wtk Keyboard Keyboard$KeyCode
                                 TableView$CellRenderer TableView$RowEditor)
           (org.apache.pivot.util Vote)
           (java.text DateFormat DecimalFormat)))

;; protocols to define editors and renderers
(defprotocol HasComponent
  (component [c] "returns the pivot component"))

(defprotocol Editor
  (on-open [e] "Called after opening the popup window")
  (value [e] "value of the edit component"))

(defprotocol Renderer
  (render [r argm] "renders the renderer"))

(defn split-key-pairs [s]
  (loop [s s
         opts {}]
    (let [[k v & t] s]
      (cond (keyword? k) (recur t (assoc opts k v))
            :else [opts s]))))

(defprotocol Stateful
  (get-state [t])
  (set-state [t s]))

(defn alter-state [t f & args]
  (set-state t (apply f (get-state t) args)))
