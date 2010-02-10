
(defproject clj-pivot "0.1.0"
    :description "A Clojure library for Apache Pivot"
    :url "http://github.com/hoeck/clj-pivot"
    :dependencies [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                   [org.clojure/clojure-contrib "1.2.0-SNAPSHOT"]
                   [org.clojars.hoeck/apache-pivot-wtk-terra "1.4"]]
    ;; compile everything except tools
    :namespaces (hoeck.pivot
                 hoeck.pivot.Application
                 hoeck.pivot.components
                 hoeck.pivot.listeners
                 hoeck.pivot.icons
                 hoeck.pivot.datastructures
                 hoeck.pivot.forms
                 hoeck.pivot.content
                 hoeck.pivot.content.table-view))
