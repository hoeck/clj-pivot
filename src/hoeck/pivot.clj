;   Copyright (c) 2009, Erik Soehnel All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns hoeck.pivot
  (:use clojure.contrib.pprint
	clojure.contrib.prxml
	clojure.contrib.duck-streams
        clojure.contrib.except

	hoeck.pivot.datastructures
        hoeck.pivot.components
        hoeck.pivot.listeners
        hoeck.pivot.tools)
  (:require [hoeck.pivot.Application :as app])
  (:import (org.apache.pivot.wtk DesktopApplicationContext)
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

(pivot-invoke #(component-inspector (@appstate :display)))

;;;

(comment

  (start-pivot appstate)
  (show-only (window (boxpane (boxpane (push-button :data "click")))))
  (pivot-invoke (component-inspector (@appstate :display)))
  (-> (get-properties (@appstate :display)) :components first)
)

