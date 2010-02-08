;   Copyright (c) 2010, Erik Soehnel All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns hoeck.pivot.Application
  (:gen-class :implements [org.apache.pivot.wtk.Application
                           org.apache.pivot.wtk.Application$UncaughtExceptionHandler]))

;; pivot startup requires a named class

(def impl (atom {}))

;; Application
(defn -startup [this display property-map]
  ((:startup @impl) display))

(defn -shutdown [this optional?]
  ;; optional? - If true, the shutdown may be canceled by returning a value of true.
  (boolean ((:shutdown @impl (constantly false)) optional?)))

(defn -suspend [this] ((:suspend @impl #())))
(defn -resume [this] ((:resume @impl #())))

;; Application$UncaughtExceptionHandler
(defn -uncaughtExceptionThrown [e]
  ((:uncaught-exception-thrown @impl #(println "Pivot: uncaught exception:" %)) e))


