
(ns hoeck.pivot
  (:use clojure.contrib.pprint
	clojure.contrib.prxml
	clojure.contrib.duck-streams
        clojure.contrib.except

	hoeck.pivot.datastructures
        hoeck.pivot.components
        hoeck.pivot.listeners)
  (:require [hoeck.pivot.Application :as app])
  (:import (org.apache.pivot.wtk Component)
           (org.apache.pivot.wtk DesktopApplicationContext)
	   (org.apache.pivot.wtkx WTKXSerializer)
           (org.apache.pivot.collections Dictionary)
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

(defn pivot-invoke [f]
  (DesktopApplicationContext/queueCallback f))

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


(defn inspector-tree [c]    
  (let [p (get-properties c)
        components (concat (:components p) (filter #(isa? (type %) Component) (vals p)))
        text (str (.getSimpleName (type c)) " " (-> p :user :name))]
    (if (not (empty? components))
      (tree-branch :text text
                   :nodes (map inspector-tree
                               components))
      (tree-node :text text))))

(defn component-inspector
  "open a component inspector in frame in the current display"  
  ([] (pivot-invoke #(component-inspector (@appstate :display))))
  ([root-component]
     (let [inspector-click-listener (listener :component-mouse-button * 
                                              #(do (println (:int %)) true))
           disp (@appstate :display)
           inspector-tv (tree-view :preferred-width 300
                                   :data (inspector-tree root-component))
           inspector-frame (frame (boxpane 
                                   :orientation :vert
                                   :user {:name 'component-listener-toplevel-box}
                                   :preferred-width 300
                                   :preferred-height 500
                                   (splitpane :preferred-size [300 500]
                                              :user {:name 'my-splitpane}
                                              :top-left (scrollpane :preferred-width 300
                                                                    :preferred-height 800
                                                                    :view inspector-tv)
                                              :orientation :vert
                                              :bottom-right (push-button :data "Detail view")
                                              :primary-region :top-left
                                              :split-ratio 0.6)))]
       (add-listener (@appstate :display) inspector-click-listener)
       (.open (frame :self inspector-frame :title "Inspector") disp))))



(comment

  (start-pivot) 
  (show-only (main-window))
  (component-inspector)

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