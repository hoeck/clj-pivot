
(ns hoeck.pivot.Application
  (:gen-class :implements [org.apache.pivot.wtk.Application]))


(def startup-fn (atom #()))

(defn set-startup-fn [f] (swap! startup-fn (constantly f)))

(defn -startup [this display property-map]
  (@startup-fn display))

(defn -shutdown [this optional?]
  ;;(when-let [w (:window @application-state)]
  ;;(.close w))
  false)

(defn -suspend [this])

(defn -resume [this])

(comment
  (require 'hoeck.pivot.Application :reload)
  (binding [*compile-path*
	    "d:/clj/clj-pivot/classes"
	    ;;"/home/timmy-turner/clj/clj-pivot/classes"
	    ]
    (compile (ns-name *ns*)))
  )

