
(ns hoeck.pivot.components.examples
  (:use hoeck.pivot.components))

(comment

  ;; playground
  (def __t  (table-view
             ;;:preferred-size [400 400]
             :row-editor :cell
             :data (make-dictionary-list '#{{:name erik :city Frankfurt :mobile-phone-number dontknow}
					    {:name frank :city Berlin :mobile-phone-number any}
					    {:name martin :city Prag :mobile-phone-number 0}})
             (table-view-column :name :name 
				:header-data "Name")
             (table-view-column :name :city
                                :header-data "Code"
                                :cell-renderer (make-cell-renderer 
						(fn ([] (PushButton.))
						  ([opts]
						     (let [v (:value opts) 
							   name (if v (.get v :name) 'null)
							   code (if v (.get v :city) 'null)]
						       (.setButtonData (:component opts) 
								       (str "call " name " in " code)))))))
             (table-view-column :name :mobile-phone-number
				:header-data "Mobile")))

  ;; table-view with header:
  (show-only (window :maximized true
                     (scrollpane :column-header (table-view-header :preferred-size [400 20]
                                                                   :table-view __t)
                                 :view __t)))

  (table-view :self __t :preferred-size [400 400]) ;; alters existing table view

  ;; form:
  (show-only (window (form :preferred-size [200 300]
                           (form-section :header "foo" 
                                         (form-component :label 'name (text-input :prompt 'vorname))
                                         (form-component :label 'nachname (text-input :prompt 'nachname)))
                           (form-section :header "Adresse" 
                                         (form-component :label 'plz (text-input :prompt 'plz))
                                         (form-component :label 'ort (text-input :prompt 'ort)))
                           (form-section :header "Sonstiges" 
                                         (form-component :label 'Bemerkungen (border 
                                                                                      
                                                                              (scrollpane
                                                                               :preferred-size [100 100]
                                                                               (text-area;;:preferred-size [100 100]
                                                                                :text "FOOOOOO"))))))))

  ;; splitpane
  (show-only (window (boxpane 
                      (splitpane
                       :preferred-size [300 200]
                       :primary-region :top-left
                       :orientation :vert
                       :split-ratio 0.9
                       :top-left (push-button :data "foobar")
                       :bottom-right (push-button :data "bakbaz")))))

  ;; accordion
  (show-only
   (window (boxpane (border (scrollpane 
                             :preferred-size [100 100]
                             :column-header (label :text "accordion" {:font ["Arial" :plain 12]})
                             :view (accordion :preferred-size [60 200]
                                              (accordion-panel :label 'one 
                                                               (boxpane :orientation :vert
                                                                        (checkbox :data "A")
                                                                        (checkbox :data "B")
                                                                        (push-button :data "clickC")))
                                              (accordion-panel :label 'bar
                                                               (push-button :data "click -me"))
                                              (accordion-panel :label 'bak 
                                                               (push-button :data "click -you"))))))))

  ;; form
  (show-only (window (border (boxpane 
                              :preferred-size [200 200]
                              (form 
                               (form-section :heading "FOO"
                                             (form-component :label 'oh
                                                             (boxpane (push-button :data "foo")
                                                                      (push-button :data "foo2")))
                                             (form-component :label 'ah (checkbox :data "bar"))
                                             (form-component :label 'ih (checkbox :data "baz")))
                               (form-section :heading "NBAR"
                                             (form-component :label 'foo (checkbox :data "bar"))
                                             (form-component :label 'foo (label :text "bar")))
                               ))))))