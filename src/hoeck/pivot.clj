
(ns hoeck.pivot
  (:use clojure.contrib.pprint
	clojure.contrib.prxml
	clojure.contrib.duck-streams
        clojure.contrib.except)
  (:require [hoeck.pivot.Application :as app]
	    [clojure.xml :as xml])
  (:import (org.apache.pivot.wtk Window 
				 Component
				 Button
				 Button$Group
				 PushButton
				 RadioButton
				 Checkbox
				 Container
				 Accordion
				 BoxPane
				 ScrollPane
				 Application
				 DesktopApplicationContext
				 Display
				 HorizontalAlignment
				 VerticalAlignment
                                 Border
				 Label
				 SortDirection
                                 Insets
				 Slider
                                 Orientation
				 TableView
                                 Dimensions
				 TableViewHeader
				 TableViewSelectionListener)
	   (org.apache.pivot.wtkx WTKXSerializer)
	   (org.apache.pivot.collections Map)
	   (java.awt Color Font)
	   (java.net URL)))

(def appstate (agent {}))

(defn start-pivot []
  (app/set-startup-fn (fn [display] (send appstate assoc :display display)))
  (DesktopApplicationContext/main hoeck.pivot.Application, (into-array String ())))

(defn get-horiz-align [keyword]
  (condp = keyword
    :left HorizontalAlignment/LEFT
    :center HorizontalAlignment/CENTER
    :right HorizontalAlignment/RIGHT))

(defn get-vert-align [keyword]
  (condp = keyword
    :top VerticalAlignment/TOP
    :center VerticalAlignment/CENTER
    :bottom VerticalAlignment/BOTTOM))

(defn dictionary->hashmap
  "Converts a dictionary into a clojure hashmap."
  [d]
  (into {} (map #(vector % (.get d %)) (seq d))))

(defn make-insets [[top left bottom right]]
  (Insets. top left bottom right))

(defn get-font-style [keyword]
  (condp = keyword
    nil Font/PLAIN
    :plain Font/PLAIN
    :bold Font/BOLD))

(def style-setters 
     {:padding ["padding" make-insets]
      :vert-align ["verticalAlignment" get-vert-align]
      :horiz-align ["horizontalAlignment" get-horiz-align]
      :spacing ["spacing" identity]
      :pref-size ["preferredSize" (fn [[x y]] (Dimensions. x y))]
      :thickness ["thickness" identity]
      :height ["height" identity]
      :width ["width" identity]
      :focusable ["focusable" identity]
      :button-padding ["buttonPadding" make-insets]
      :font ["font" (fn [[name style size]] (Font. name (get-font-style style) size))]})

(defn set-styles
  "set the style of a component"
  [component style-map]
  (let [sd (.getStyles component)]
    (doseq [[k v] style-map]
      (let [[key f] (style-setters k)]
        (.put sd key (f v))))
    component))

(defn get-orientation [keyword]
  (condp = keyword 
    :horiz Orientation/HORIZONTAL
    :vert Orientation/VERTICAL))

(defn get-icon [key]
  ;;look icon up in the gobal image map an return an image or nil or throw sth.
  )



(defn show-only [window]
  (if-let [disp (:display @appstate)]
    (do (.removeAll disp)
	(.open window disp))
    (throwf "no display available")))

;; (window (boxpane (border (boxpane (label :text "hallo")
;; 				  (accordion {:color :red}
;; 					     (accordion-pane :icon :icon1
;; 							     :label bar
;; 							     (button :text "world"
;; 								     :text foo)))))))

;; another try
(defn parse-component-ctor-args
  "keywords, style-map, components"
  [arglist]
  (loop [a arglist
	 mode :keyargs
	 keyargs {}
	 style {}
	 components []
	 other []]
    (if (seq a)
      (condp = mode 
	:keyargs (let [[k v & more] a]
		   (if (keyword? k)
		     (recur more mode (assoc keyargs k v) style components other)
		     (recur a :style keyargs style components other)))
	:style (let [[s & more] a]
		 (if (map? s)
		   (recur more :components keyargs s components other)
		   (recur a :components keyargs style components other)))
	:components (let [[c & more] a]
		      (if (isa? (type c) Component)
			(recur more mode keyargs style (conj components c) other)
			(recur more mode keyargs style components (conj other c)))))
      {:style style :args keyargs :components components :other-args other})))

;;(parse-args [:a 1 :b 2 {:style 'foo} (BoxPane.) (Border.) (Border.) 'foo])

(defmacro def-component-ctor
  "Define a component constructor. Arg-keys is a vector of symbols naming
  keys from the arguments parsed with parse-component-ctor-args."
  [name arg-keys & body]
  `(defn ~name [& args#]
     (let [{:keys ~arg-keys} (parse-component-ctor-args args#)]
       ~@body)))

(defmacro when-it
  "Anaphoric when using ìt'."
  [expr & body]
  `(when-let [~'it ~expr] ~@body))

(defmacro cond-call-it
  "(when (contains? args :keyword) (let [it (get args :keyword)] (.doSomething foo it)))
   ...
   ->
  (cond-call-it args
    :keyword (.doSomething foo it)
    ...)."
  [args & clauses]
  (let [args_ (gensym 'args)]
    `(let [~args_ ~args]
       ~@(map (fn [[key expr]]
		`(when (contains? ~args_ ~key)
		   (let [~'it (get ~args ~key)]
		     ~expr)))
	      (partition 2 clauses)))))

(defn default-component-arg-handler
  "handler for common component args, returns comp"
  [comp args]
  (cond-call-it args
    :enabled (.setEnabled comp it)
    :height (.setHeight comp it)
    :width (.setWidth comp it)
    :preferred-width (if (vector? it) 
		       (let [[min max] it] (.setPreferredWidthLimits comp min max))
		       (.setPreferredWidth comp it))
    :preferred-height (if (vector? it)
			(let [[min max] it] (.setPreferredHeightLimits comp min max))
			(.setPreferredHeight comp it))
    :preferred-size (let [[x y] it] (.setPreferredSize comp x y)))
  comp)

(defmacro with-component
  "For use with in the def-component-ctor macro. Requires `args' and `styles'
  to be bound to the keyargs and styles maps. Executes body with in a component
  bound to component-name, invokes default-component-arg-handler on component and
  returns the component.
  Component is eiter instanciated using the defalt classname ctor or by looking up
  :self in args."
  [[component-name classname] & body]
  `(let [~component-name (default-component-arg-handler 
			   (set-styles (or (:self ~'args) (new ~classname))
				       ~'style)
			   ~'args)]
     ~@body
     ~component-name))

;; containers

(def-component-ctor window [args style components]
  (with-component [w Window]
    (when-let [[c] components] (.setContent w c))
    (cond-call-it args
      :maximized (.setMaximized w it)
      :visible (.setVisible w it)
      :title (.setTitle w it)
      :icon (.setIcon w (get-icon it)))))

(def-component-ctor boxpane [args style components]
  (with-component [bp BoxPane]
    (doseq [c components] (.add bp c))
    (when-it (:orientation args) (.setOrientation bp (get-orientation it)))))

(def-component-ctor border [args style components]
  (with-component [bo Border]
    (when-let [[c] components] (.setContent bo c))
    (when-it (:title args) (.setTitle bo it))))

(def-component-ctor accordion [args style components]
  (with-component [acc Accordion]
    (let [p (.getPanels acc)]
      (doseq [c components]
	(.add p c)))
    (cond-call-it args
	:icons (dorun (map (fn [p i] (Accordion/setIcon p (get-icon i)))
			   (.getPanels acc)
			   it))
	:labels (dorun (map (fn [p l] (Accordion/setLabel p (str l)))
			    (.getPanels acc)
			    it))
	:selected-index (.setSelectedIndex acc it))))

(def-component-ctor scrollpane [args style components]
  (with-component [sc ScrollPane]
    (doseq [c components] (.add sc c))
    (cond-call-it args
      :consume-repaint (.setConsumeRepaint sc it))))

(def-component-ctor slider [args style components] ;; why is a slider a container?
  (with-component [sl Slider]
    (doseq [c components] (.add sl c))
    (cond-call-it args
      :range (let [[min max] it] (.setRange sl min max)))))

;; components

(def-component-ctor label [args style]
  (with-component [la Label]
    (when-it (:text args) (.setText la it))))

(defn button-arg-handler
  "handler for common button args"
  [button args]
  (cond-call-it args
      :group (.setGroup button (if (isa? (type it) Button$Group)
				 it
				 (str it)))
      :data (.setButtonData button it)
      :selected (.setSelected button it)))

(def-component-ctor push-button [args style]
  (with-component [pb PushButton]
    (button-arg-handler pb args)))

(def-component-ctor radio-button [args style]
  (with-component [rb RadioButton]
    (button-arg-handler rb args)))

(def-component-ctor checkbox [args style]
  (with-component [cb Checkbox]
    (button-arg-handler cb args)))

;;(show-only (window (label :self (Label.) :text "Hallo YOU" {:font ["Arial" :bold 70]})))

(comment
  (show-only
   (window (boxpane (border (boxpane			     
			     (label :text "accordion" {:font ["Arial" :bold 70]})
			     (accordion :preferred-size [100 400]
					:labels ['one 'two 'three]
					(boxpane :orientation :vert
						 (checkbox :data "clickA")
						 (checkbox :data "clickB")
						 (push-button :data "clickC"))
					(push-button :data "click -me")
					(push-button :data "click -you"))))))))
