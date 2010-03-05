;   Copyright (c) 2010, Erik Soehnel All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns hoeck.pivot
  (:refer-clojure :exclude [->])
  (:use clojure.contrib.pprint
        clojure.contrib.except)
  (:require [hoeck.pivot.Application :as app])
  (:import (org.apache.pivot.wtk DesktopApplicationContext
                                 Window Component)
	   (java.net URL)))

(def state (atom {})) ;; the root component

(defn init
  "return a promise that contains a display when pivot is ready."
  []
  (let [startup-p (promise)]
    (swap! app/impl assoc :startup #(deliver startup-p %))
    (DesktopApplicationContext/main hoeck.pivot.Application, (into-array String ()))
    startup-p))

(defn setup
  "open a pivot window"
  [] (reset! state {:display @(init)}))

(defn invoke [f]
  (DesktopApplicationContext/queueCallback f))

(deftype exception-delegating-promise [result]
  clojure.lang.IDeref
  (deref []
    (let [r (deref result)]
      (if (instance? Throwable r)
        (throw r)
        (nth r 0)))))

(defmacro pivot-do
  "Execute body in the pivot thread."
  [& body]
  `(let [result# (promise)]
     (invoke #(deliver result#
                       (try [(do ~@body)]
                            (catch Throwable t# t#))))
     (exception-delegating-promise result#)))

(defmacro ->
  "Same as clojure.core/->, but the first form is a pivot-display and the
  threaded body is executed within the pivot EDT.
  Returns a promise of its result."
  [& body]
  `(pivot-do (clojure.core/-> (:display @state)
                         ~@body)))

(defn show
  "show a single window using the current display"
  [disp thing]
  (.removeAll disp)
  (.open (condp  instance? thing
             Window thing
             Component (doto (Window. thing) (.setMaximized true))
             (throwf "thing must be a Window or a component"))
         disp))

