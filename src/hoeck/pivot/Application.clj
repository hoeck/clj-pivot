
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


