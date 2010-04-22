;   Copyright (c) 2010, Erik Soehnel All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns hoeck.pivot.examples
  (:require [hoeck.pivot :as pivot])
  (:use hoeck.pivot.components
        hoeck.pivot.listeners
        hoeck.pivot.content.table-view)
  (:import (org.apache.pivot.wtk Application
                                 Application$UncaughtExceptionHandler)))

(defn example-shelf [left bottom component]
  (table-pane
   :cols [-1 [1]]
   (table-pane-row :height [1]
                   (label :styles {:vertical-alignment :center}
                                      left)
                   component)
   (table-pane-row [(label :styles {:horizontal-alignment :center}
                           bottom)
                    [2 *]])))

(defn example-tv []
  (let [data #{{:name "name1" :id 10}
               {:name "name2" :id 20}
               {:name "name3" :id 30}}
        tv (table-view
            :data data
            (table-view-column :name :name
                               :header-data "Name"
                               :renderer text-renderer)
            (table-view-column :name :id
                               :header-data "ID"
                               :renderer text-renderer))]
    (scrollpane :column-header (table-view-header :preferred-size [400 20]
                                                  :table-view tv)
                :view tv)))

(defn example-form []
  (form :preferred-size [200 300]
        (form-section :header "foo"
                      (form-component :label 'Name (text-input :prompt 'Name :text-key :name))
                      (form-component :label 'ID (text-input :prompt 'ID :text-key :id)))
        (form-section :header "Adress" 
                      (form-component :label 'Zip (text-input :prompt 'Zip :text-key :zip))
                      (form-component :label 'City (text-input :prompt 'City :text-key :city)))
        (form-section :header "Remarks"
                      (form-component (border
                                       (scrollpane
                                        :preferred-size [230 100]
                                        :view (text-area :text-key :remarks
                                                         :preferred-size [200 300])))))))

(defn button-box []
  (boxpane :orientation :vert
           :styles {:fill true}
           (push-button :data "1")
           (push-button :data "2")
           (push-button :data "3")
           (push-button :data "4")))

(defn run-example [display]
  ;;(pivot/show display (example-shelf "left" "table-view-example" (example-tv)))
  ;;(pivot/show display (example-shelf "left" "form-example" (example-form)))
  (pivot/show display (example-shelf "left" "container-example"
                                     (border
                                       :styles {:padding [4 4 4 4]}
                                       (button-box)))))

(deftype ExampleApp []
  Application
  (startup [this display property-map]
           (require 'hoeck.pivot.examples)
           (hoeck.pivot.examples/run-example display))
  (shutdown [this optional?]
            ;; optional? - If true, the shutdown may be canceled by returning a value of true.
            ;; return-value: true to cancel shutdown, false to continue.
            false)
  (suspend [this])
  (resume [this])
  Application$UncaughtExceptionHandler
  (uncaughtExceptionThrown [this e]
                           (println e)))

;; either invoke Example app from the commandline (after compiling with lein compile):
;; java -cp classes/:lib/* org.apache.pivot.wtk.DesktopApplicationContext hoeck.pivot.examples.ExampleApp
;; or use the start function
;; (pivot/start run-example)

