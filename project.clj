(defproject clj-pivot "0.2.0"
  :description "A Clojure library for Apache Pivot"
  :url "http://github.com/hoeck/clj-pivot"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.apache.pivot/pivot-wtk "1.5.1"]
                 [org.apache.pivot/pivot-wtk-terra "1.5.1"]]
  :namespaces [hoeck.pivot
               hoeck.pivot.components.TreeBranch
               hoeck.pivot.components
               hoeck.pivot.listeners
               hoeck.pivot.forms
               hoeck.pivot.icons
               hoeck.pivot.datastructures
               hoeck.pivot.content
               hoeck.pivot.content.table-view
               hoeck.pivot.Application])


