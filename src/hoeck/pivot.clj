
(ns hoeck.pivot
  (:use clojure.contrib.pprint
	clojure.contrib.prxml
	clojure.contrib.duck-streams
        clojure.contrib.except

	hoeck.pivot.datastructures
        hoeck.pivot.components
        hoeck.pivot.listeners)
  (:require [hoeck.pivot.Application :as app])
  (:import (org.apache.pivot.wtk DesktopApplicationContext)
	   (org.apache.pivot.wtkx WTKXSerializer)
	   (java.net URL)))

(def appstate (agent {}))

(defn show-only
  "show a single window using the current display"
  [window]
  (if-let [disp (:display @appstate)]
    (do (.removeAll disp)
	(.open window disp))
    (throwf "no display available")))

(defn start-pivot []
  (app/set-startup-fn (fn [display] (send appstate assoc :display display)))
  (DesktopApplicationContext/main hoeck.pivot.Application, (into-array String ())))

;;DesktopApplicationContext/queueCallback(Runnable callback) ;; to execute things within the pivot (awt) thread

(defn accordion-menu []
  (accordion :preferred-size [120 300]
	       (accordion-panel :label 'one 
				(boxpane :orientation :vert
					 (checkbox :data "A")
					 (checkbox :data "B")
					 (push-button :data "clickC")))
	       (accordion-panel :label 'bar
				(push-button :data "click -me"))
	       (accordion-panel :label 'bak 
				(push-button :data "click -you"))))

(defn main-window []
  (window :maximized true
	  (boxpane :orientation :horiz
		   (boxpane :user {:name 'left-box}
			    (accordion-menu))
		   (boxpane :user {:name 'main-pane})
		   
		   )))



(defn component-inspector []
  (let [inspector-click-listener (listener :component-mouse-button * 
					   #(do (println (:int %)) true))]
    (add-listener (@appstate :display) inspector-click-listener)))

(defn tree-view-from-hashmap [x]
  (tree-view :data (make-list 'a 'b 'c 'd))
  )

(comment
  (start-pivot)
  
  (let [w (window :maximized true
		  (label :text "hier"))]
    (show-only w)
    (.open (frame (push-button  :data "clickme"))
	   (@appstate :display)))

  (show-only (main-window))

  (component-inspector)
  (remove-listeners (@appstate :display))

  
  (show-only (window (tree-view :data (tree-branch (tree-node :text 'foo)
						   (tree-branch :text 'please-expand!
								(tree-node :text 'bar-0)
								(tree-node :text 'bar-1)
								(tree-node :text 'bar-2)
								(tree-node :text 'bar-3))
						   (tree-node :text 'baz)))))

  
  (show-only (window (.readObject (WTKXSerializer.) (URL. "file:///d:/clj/trees.wtkx"))))
  (let [X (.readObject (WTKXSerializer.) (URL. "file:///d:/clj/trees.wtkx"))]
    (-> X get-properties :components first get-properties :components first get-properties :components
	second get-properties :components first get-properties :components first get-properties
	pprint)
    )
  )

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