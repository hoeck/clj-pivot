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

	hoeck.pivot.datastructures
        hoeck.pivot.components
        hoeck.pivot.listeners
        hoeck.pivot.tools)
  (:require [hoeck.pivot.Application :as app])
  (:import (org.apache.pivot.wtk DesktopApplicationContext)
           (org.apache.pivot.wtkx WTKXSerializer)
           (org.apache.pivot.collections Dictionary)
	   (java.net URL)))

(def appstate (agent {}))

(defn show-only
  "show a single window using the current display"
  [window]
  (if-let [disp (:display @appstate)]
    (do (.removeAll disp)
	(.open window disp))
    (throwf "no display available")))

(defn start-pivot [agent]
  (app/set-startup-fn (fn [display] (send agent assoc :display display)))
  (DesktopApplicationContext/main hoeck.pivot.Application, (into-array String ())))

(defn pivot-invoke [f]
  (DesktopApplicationContext/queueCallback f))

;;;

(comment

  (start-pivot appstate)
  (show-only (window (boxpane (boxpane (push-button :data "click")))))

  (show-only (.readObject (WTKXSerializer.) (ClassLoader/getSystemResource "table_panes.wtkx")))
  
  (pivot-invoke #(component-inspector (@appstate :display)))
  (-> (get-properties (@appstate :display)) :components first)
  

  (show-only (window 
              :maximized true
              (boxpane
               :styles {:fill true}
               (border
                (table-pane :cols [[1] [1] [1] [10]]
                            (table-pane-row (label :text "das")
                                            (label :text "ist")
                                            (label :text "eine")
                                            (label :text "zeile"))
                            (table-pane-row (push-button :data "mit")
                                            (push-button :data "vier")
                                            (push-button :data "push")
                                            (push-button :data "Buttons")))))))
  
)

