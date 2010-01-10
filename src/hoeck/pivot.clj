;   Copyright (c) 2009, Erik Soehnel All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns hoeck.pivot
  (:use clojure.contrib.pprint
        clojure.contrib.except
        hoeck.pivot.content
	hoeck.pivot.datastructures
        hoeck.pivot.components
        hoeck.pivot.listeners
        hoeck.pivot.tools)
  (:require [hoeck.pivot.Application :as app])
  (:import (org.apache.pivot.wtk DesktopApplicationContext)
           (org.apache.pivot.wtkx WTKXSerializer)
           (org.apache.pivot.collections Dictionary)
	   (java.net URL)
           (hoeck.pivot AWTExceptionHandler)))

(def appstate (atom {}))

(defn show-only
  "show a single window using the current display"
  [window]
  (if-let [disp (:display @appstate)]
    (do (.removeAll disp)
	(.open window disp))
    (throwf "no display available")))

(defn start-pivot
  "return a promise that contains a display when pivot is ready."
  []
  (let [startup-p (promise)]
    (app/set-startup-fn (fn [display] (deliver startup-p display)))
    (DesktopApplicationContext/main hoeck.pivot.Application, (into-array String ()))
    startup-p))

(defn pivot-invoke [f]
  (DesktopApplicationContext/queueCallback f))

(defmacro with-pivot
  "Execute body in the pivot thread."
  [& body] `(pivot-invoke (fn [] ~@body)))

;;;

(comment

  (start-pivot appstate)
  (show-only (window (boxpane (boxpane (push-button :data "click")))))

  (show-only (.readObject (WTKXSerializer.) (ClassLoader/getSystemResource "table_panes.wtkx")))
  
  (pivot-invoke #(component-inspector (@appstate :display)))

  (-> (get-properties (@appstate :display)) :components first)
  
  (set-properties @selected-component {:preferred-width [20 200 *]})
  (show-only (window 
              :maximized true
              (boxpane
               :styles {:fill true}
               (border
                (table-pane :cols [[1] [1] [1] [10]]
                            (table-pane-row (label :text "this")
                                            (label :text "is")
                                            (label :text "a")
                                            (label :text "long row"))
                            (table-pane-row (push-button :data "with")
                                            (push-button :data "four")
                                            (push-button :data "push")
                                            (push-button :data "buttons")))))))
  
)

