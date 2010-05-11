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

;; (defn- find-ctor-def
;;   "Search a deftype body for a ctor definition and return it."
;;   [macro-body]
;;   (loop [b macro-body
;;          body nil]
;;     (let [[h & t] b]
;;       (cond (nil? b) [nil body]
;;             (keyword? h) (recur (next t) (concat body (list h) (first t)))
;;             (symbol? h) (recur t b)
;;             (seq? h) (if (or (vector? (first h)) (seq? (first h)))
;;                        [h (concat body t)]
;;                        (recur t (concat body (list h))))
;;             :else (throw "no matching clause in find-ctor-code")))))

;; (defmacro deftypec
;;   "Creates a type with a custom constructor function. The constructor is
;;   a method form without a name in the regular deftype body."
;;   [name fields & body] ;; nonstandard ctor
;;   (let [[ctor-def deftype-body] (find-ctor-def body)]
;;     `(do (deftype ~name ~fields ~@deftype-body)
;;          (let [original-ctor# ~name
;;                nonstandard-ctor# (fn ~@ctor-def)]
;;            (defn ~name [& args#]
;;              (clojure.lang.Reflector/invokeConstructor
;;               ~name
;;               (into-array Object (apply nonstandard-ctor# args#))))))))


