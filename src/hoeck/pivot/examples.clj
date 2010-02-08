
(ns hoeck.pivot.examples
  (:require [hoeck.pivot :as pivot])
  (:use hoeck.pivot.components
        hoeck.pivot.listeners
        hoeck.pivot.content.table-view))

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

(comment
  (pivot/setup)
  (pivot/-> (pivot/show (example-shelf "left" "table-view-example" (example-tv))))
  (pivot/-> (pivot/show (example-shelf "left" "form-example" (example-form))))
  (pivot/-> (pivot/show (example-shelf "left" "container-example"
                                       (border
                                        :styles {:padding [4 4 4 4]}
                                        (button-box))))))

