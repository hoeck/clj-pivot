;   Copyright (c) 2010, Erik Soehnel All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns hoeck.pivot
  (:use clojure.contrib.except)
  (:require [hoeck.pivot.Application :as app])
  (:import (org.apache.pivot.wtk Application
                                 Application$UncaughtExceptionHandler
                                 ApplicationContext
                                 DesktopApplicationContext
                                 Window Component)
	   (java.net URL)))

(def clojure-application-fn (atom nil))
(deftype ClojureApplication []
  Application
  (startup [this display property-map]
           (@clojure-application-fn display))
  (shutdown [this optional?]
            ;; optional? - If true, the shutdown may be canceled by returning a value of true.
            ;; return-value: true to cancel shutdown, false to continue.
            false)
  (suspend [this])
  (resume [this])
  Application$UncaughtExceptionHandler
  (uncaughtExceptionThrown [this e]
                           (println e)))

(defn start
  "Given a class or a string, use that class to start pivot
  (which must implement org.apache.pivot.Application)
  using a DesktopApplicationContext. This works only for AOT-compiled classes.
  Given a function, start pivot and invoke the function with the pivot display.
  Use the latter approach for starting pivot during development."
  [app-class-or-fn & args]
  (cond (class? app-class-or-fn)
          (DesktopApplicationContext/main app-class-or-fn, (into-array String (map str args)))
        (string? app-class-or-fn)
          (DesktopApplicationContext/main (into-array String (cons app-class-or-fn (map str args))))
        (fn? app-class-or-fn)
          (do (reset! clojure-application-fn app-class-or-fn)
              (start ClojureApplication))))

(defn invoke
  "Queue and invoke the given function within the pivot thread.
  Always returns nil"
  [f]
  (ApplicationContext/queueCallback f))

(deftype ExceptionDelegatingPromise [result]
  clojure.lang.IDeref
  (deref [this]
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
     (ExceptionDelegatingPromise. result#)))

(defn show
  "show a single window using the current display"
  [disp thing]
  (.removeAll disp)
  (.open (condp  instance? thing
           Window thing
           Component (doto (Window. thing) (.setMaximized true))
           (throwf "thing must be a Window or a component"))
         disp))

