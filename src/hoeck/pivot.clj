;   Copyright (c) 2010, Erik Soehnel All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns hoeck.pivot
  (:import (org.apache.pivot.wtk Application
                                 Application$UncaughtExceptionHandler
                                 ApplicationContext
                                 DesktopApplicationContext
                                 Window Component)))


(def display nil) ;; the root component
(def event-dispatch-thread nil)

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

(defn pivot-do*
  "Invoke function f in the pivot event-dispatch-thread by using pivot/invoke.
  If pivot-do* is already called from the event-dispatch-thread,
  invoke it directly without queuing.
  Returns a promise containing the result.
  Throw exceptions occuring during the evaluation of body in the
  in the calling thread (after derefering the promise)."
  [f]
  (let [result (promise)]
    (if (= (Thread/currentThread) event-dispatch-thread)
      (deliver result (f))
      (invoke #(deliver result
                        (try [(f)]
                             (catch Throwable t t)))))
    (ExceptionDelegatingPromise. result)))

(defn pivot-do [& body]
  "Wrapper macro around pivot-do*."
  `(pivot-do* (fn [] ~@body)))

(defn start
  "Open a pivot display and store the created display in the `display' Var.
  Use this function to start pivot from a repl or a repl like environment."
  [& args]
  (DesktopApplicationContext/main (into-array String (cons "hoeck.pivot.Application" (map str args)))))

(defn show
  "show a single window using the given display."
  [disp thing]
  (.removeAll disp)
  (.open (condp  instance? thing
           Window thing
           Component (doto (Window. thing) (.setMaximized true))
           (throw (Exception. "thing must be a Window or a component")))
         disp))

(defmacro disp->
  "like ->, but threads in the current pivot display and wraps the
  execution of body in a pivot do. To retrieve the result of body, use deref.
  Does not execute body if the display is nil."
  [& body]
  `(pivot-do (when-let [d# display] (-> d# ~@body))))

