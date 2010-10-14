;   Copyright (c) 2010, Erik Soehnel All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns hoeck.pivot.Application
  (:require hoeck.pivot)
  (:import [org.apache.pivot.wtk DesktopApplicationContext])
  (:gen-class :implements [org.apache.pivot.wtk.Application
                           org.apache.pivot.wtk.Application$UncaughtExceptionHandler]
              :main true))

;; pivot startup requires a named class

(def impl (ref {}))

;; Application
(defn -startup [this disp property-map]
  ;; excuse: pivot/display is only set once when starting pivot
  (require 'hoeck.pivot)
  (alter-var-root #'hoeck.pivot/display (constantly disp))
  (alter-var-root #'hoeck.pivot/event-dispatch-thread (constantly (Thread/currentThread)))
  (when-let [rq (or
                 ;; use a commandline argument like:
                 ;; --require=foo.bar.namespace
                 (.get property-map "require")
                 ;; or a property file named `startup.properties'
                 ;; located in a classpath root with the contents:
                 ;; hoeck.pivot.require=foo.bar.namespace
                 (when-let [s (.getResourceAsStream
                               (clojure.lang.RT/baseLoader)
                               "startup.properties")]
                   (let [properties (doto (new java.util.Properties)
                                      (.load s))]
                     (.getProperty properties "hoeck.pivot.require"))))]
    (require (symbol rq))))

(defn -shutdown [this optional?]
  ;; optional? - If true, the shutdown may be canceled by returning a value of true.
  (boolean ((:shutdown @impl (constantly false)) optional?)))

(defn -suspend [this] ((:suspend @impl #())))
(defn -resume [this] ((:resume @impl #())))

;; Application$UncaughtExceptionHandler
(def last-uncaught-exception (atom nil))
(defn -uncaughtExceptionThrown [this e]
  ((:uncaught-exception-thrown @impl
                               #(do (reset! last-uncaught-exception %)
                                    (println "uncaught exception:" %))) e))

(defn -main [& args]
  (DesktopApplicationContext/main (into-array String (cons "hoeck.pivot.Application" (map str args)))))

