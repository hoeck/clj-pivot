
(ns hoeck.pivot.AWTExceptionHandler
  (:gen-class :methods [[handle [Throwable] void]]))

(def current-handler
     (atom (fn [t] 
             (println "awt-exception:"
                      (.getName (class t))
                      (.getMessage t)))))

(defn -handle [this t]
  (try (@current-handler t)
       (catch Throwable e nil)))

(defn register-exception-handler []
  (System/setProperty "sun.awt.exception.handler"
                      "hoeck.pivot.AWTExceptionHandler"))

(register-exception-handler)

