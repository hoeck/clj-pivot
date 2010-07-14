
;; augmenting TreeBranch with userData
(ns hoeck.pivot.components.TreeBranch
  (:gen-class :extends org.apache.pivot.wtk.content.TreeBranch
              :methods [[getUserData [] clojure.lang.IPersistentMap]
                        [setUserData [clojure.lang.IPersistentMap] clojure.lang.IPersistentMap]]
              :init init
              :state state))

(defn -init [] [[] (atom {})])
(defn -setUserData [t m] (swap! (.state t) merge m))
(defn -getUserData [t] @(.state t))

