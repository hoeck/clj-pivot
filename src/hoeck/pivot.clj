
(ns hoeck.pivot
  (:use clojure.contrib.pprint
	clojure.contrib.prxml
	clojure.contrib.duck-streams
        clojure.contrib.except)
  (:require [hoeck.pivot.Application :as app]
	    [clojure.xml :as xml])
  (:import (org.apache.pivot.wtk DesktopApplicationContext Application Display
                                 ;; containers
				 Container
                                 Window
                                 Form Form$Section
                                 SplitPane SplitPane$Region
                                 TabPane Accordion BoxPane Border
				 ScrollPane ScrollPane$ScrollBarPolicy Panorama
                                 ;; components
                                 Component 
                                 Label Slider
				 Button Button$Group PushButton RadioButton Checkbox
                                 TextInput TextArea
                                 ;; tables
                                 TableView TableViewHeader TableView$SelectMode TableView$Column TableView$CellRenderer
                                 ;; enums, structs
                                 Orientation SortDirection Insets Dimensions VerticalAlignment HorizontalAlignment)
           
           (org.apache.pivot.wtk.content TableViewBooleanCellRenderer 
                                         TableViewCellRenderer
                                         TableViewDateCellRenderer
                                         TableViewFileSizeCellRenderer
                                         TableViewImageCellRenderer
                                         TableViewMultiCellRenderer
                                         TableViewNumberCellRenderer
                                         TableViewCellEditor
                                         TableViewRowEditor)
           (org.apache.pivot.wtk.text.validation Validator)
           (org.apache.pivot.util Filter)
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

(defn get-scrollbar-policy
  "Mapping from keywords to ScrollPane$ScrollBarPolicy enums."
  [key]
  (condp = key
    :always ScrollPane$ScrollBarPolicy/ALWAYS
    :auto ScrollPane$ScrollBarPolicy/AUTO
    :fill ScrollPane$ScrollBarPolicy/FILL
    :fill-to-capacity ScrollPane$ScrollBarPolicy/FILL_TO_CAPACITY
    :never ScrollPane$ScrollBarPolicy/NEVER))

(defn make-validator
  "Return an org.apache.pivot.wtk.text.validation.Validator.
  f must be a function of at least one arg, which is called with
  a String and should return logical true if the string is valid."
  [f]
  (proxy [Validator] []
    (isValid [text] (if (f text) true false))))

(defn get-table-view-select-mode [keyword]
  (condp = keyword
    :multi TableView$SelectMode/MULTI
    :none TableView$SelectMode/NONE
    :single TableView$SelectMode/SINGLE))

(defn get-sort-direction [keyword]
  (condp = keyword
    :asc SortDirection/ASCENDING
    :desc SortDirection/DESCENDING))

(defn make-filter
  "Return a org.apache.pivot.util.Filter with the function f performing
  the boolean include(Object item) method. f should return logical true for
  item to be included."
  [f]
  (proxy [Filter] []
    (include [item] (if (f item) true false))))

(defn make-table-view-cell-renderer
  "Return a TableView$CellRenderer, defaults (= arg nil) to
  one that renders strings."
  [arg]
  (if (or (nil? arg) (keyword? arg))
    (condp = (or arg :string)
      :boolean (TableViewBooleanCellRenderer.)
      :string (TableViewCellRenderer.)
      :date (TableViewDateCellRenderer.)
      :filesize (TableViewFileSizeCellRenderer.)
      :image (TableViewImageCellRenderer.)
      :multi (TableViewMultiCellRenderer.)
      :number (TableViewNumberCellRenderer.))
    arg))

(defn make-table-view-editor [key]
  (condp = key
    :row (TableViewRowEditor.)
    :cell (TableViewCellEditor.)))


;; show a single window

(defn show-only [window]
  (if-let [disp (:display @appstate)]
    (do (.removeAll disp)
	(.open window disp))
    (throwf "no display available")))


;; tools for component constructors

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
		      (recur more mode keyargs style (conj components c) other)))
      {:style style :args keyargs :components components})))

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
    (doseq [acc-panel-f components]
      (acc-panel-f acc))
    (cond-call-it args
      :selected-index (.setSelectedIndex acc it))))

(def-component-ctor accordion-panel [args components]
  (let [[c] components]
    (fn [accordion]
      (.add (.getPanels accordion) c)
      (cond-call-it args
        :label (Accordion/setLabel c (str it))
        :icon (Accordion/setIcon c (get-icon it)))
      c)))

;; forms

(def-component-ctor form [args style components]
  (with-component [fm Form]
    (doseq [add-section-f components]
      (add-section-f fm))))

(def-component-ctor form-section [args components]
  (fn [form]
    (let [sec (or (:self args) (Form$Section.))]
      (.add (.getSections form) sec)
      (doseq [add-form-component-f components]
        (add-form-component-f sec))
      (cond-call-it args
        :heading (.setHeading sec (str it))))))

(def-component-ctor form-component [args components]
  (let [[c] components]
    (fn [section]
      (.add section c)
      (cond-call-it args
        :label (Form/setLabel c (str it)))
      c)))

;;(form (form-section :header 'blah 
;;                    (form-component :label .. component)
;;                    (form-component :label .. component))
;;      (form-section :header 'blah
;;                    (form-component :label .. component)
;;                    (form-component :label .. component)))

;; tabs

(def-component-ctor tabpane [args style components]
  (with-component [ta TabPane]
    (let [tab-sequence (.getTabs ta)]
      (doseq [add-tab-f components]
        (add-tab-f tab-sequence)))
    (cond-call-it args
      :corner (.setCorner ta it)
      :selected-index (.setSelectedIndex ta it))))

(def-component-ctor tabpane-component [args components]
  (let [[c] components]
    (fn [tab-sequence]
      (.add tab-sequence c)
      (cond-call-it args
        :closeable (TabPane/setCloseable c it)
        :label (TabPane/setLabel c it)
        :icon (TabPane/setIcon c (get-icon it))))))

;;(show-only (window (tabpane (tabpane-component :label "FOO"
;;                                               (boxpane :orientation :vert
;;                                                        (push-button :data 'A)
;;                                                        (push-button :data 'D)))
;;                            (tabpane-component :label "BAR"
;;                                               (boxpane :orientation :horiz
;;                                                        (push-button :data 'B)
;;                                                        (push-button :data 'C))))))

(def-component-ctor splitpane [args style]
  (with-component [sp SplitPane]
    (cond-call-it args
      :locked (.setLocked sp it)
      :orientation (.setOrientation sp (get-orientation it))
      :primary-region (.setPrimaryRegion sp (condp = it
                                              :top-left SplitPane$Region/TOP_LEFT
                                              :bottom-right SplitPane$Region/BOTTOM_RIGHT))
      :split-ratio (.setSplitRatio sp it)
      ;; components
      :bottom (.setBottom sp it)
      :bottom-right (.setBottomRight sp it)
      :left (.setLeft sp it)
      :right (.setRight sp it)
      :top (.setTop sp it)
      :top-left (.setTopLeft sp it))))


;; views

(def-component-ctor scrollpane [args style components]
  (with-component [sc ScrollPane]
    (doseq [c components] (.add sc c))
    (cond-call-it args
      :horiz-scrollbar-policy (.setHorizontalScrollBarPolicy sc (get-scrollbar-policy it))
      :vert-scrollbar-policy (.setVerticalScrollBarPolicy sc (get-scrollbar-policy it))
      :consume-repaint (.setConsumeRepaint sc it)
      :scroll-left (.setScrollLeft sc it)
      :scroll-top (.setScrollTop sc it)
      ;; components
      :corner (.setCorner sc it)
      :row-header (.setRowHeader sc it)
      :column-header (.setColumnHeader sc it)
      :view (.setView sc it))))

(def-component-ctor panorama [args style components]
  (with-component [pa Panorama]
    (doseq [c components] (.add pa c))
    (cond-call-it args
      :consume-repaint (.setConsumeRepaint pa it)
      :scroll-left (.setScrollLeft pa it)
      :scroll-top (.setScrollTop pa it)
      ;; components
      :view (.setView pa it))))


;; components

(def-component-ctor slider [args style components] ;; why is a slider a container?
  (with-component [sl Slider]
    (doseq [c components] (.add sl c))
    (cond-call-it args
      :range (let [[min max] it] (.setRange sl min max)))))

;; text

(def-component-ctor label [args style]
  (with-component [la Label]
    (when-it (:text args) (.setText la it))))

(def-component-ctor text-input [args style components]
  (with-component [ti TextInput]
    (cond-call-it args
      :max-length (.setMaximumLength ti it)
      :password (.setPassword ti it)
      :prompt (.setPrompt ti (str it))
      :selection (let [[s e] it] (.setSelection ti s e))
      :text (.setText ti (str it))
      :text-key (.setTextKey ti (str it))
      :text-node (.setTextNode ti it)
      :text-size (.setTextSize ti it)
      :validator (.setValidator ti (make-validator it)))))

(def-component-ctor text-area [args style components]
  (with-component [ta TextArea]
    (cond-call-it args 
      :editable (.setEditable ta it)
      :selection (let [[s e] it] (.setSelection ta s e))
      :text (.setText ta (str it))
      :text-key (.setTextKey ta (str it)))))

;; tables

(def-component-ctor table-view [args style components] ;; actually, components are TableView$Columns
  (with-component [t TableView]
    (let [col-sequence (.getColumns t)]
      (doseq [column components]
        (.add col-sequence column)))
    (cond-call-it args
      :disabled-filter (.setDisabledRowFilter t (make-filter it))
      :row-editor (.setRowEditor t (make-table-view-editor it))
      :selected-index (if (number? it) 
                        (.setSelectedIndex t it)
                        (let [[s e] it] (.setSelectedRange t s e))) ;; TODO: add support for ranges [[s0 e0] [s1 e1] ..]
      :select-mode (.setSelectMode t (get-table-view-select-mode it))
      :data (.setTableData t it))))

(def-component-ctor table-view-column [args style] ;; actually, not a component
  (let [tv (or (:self args) (TableView$Column.))]
    (cond-call-it args
      :filter (.setFilter tv (make-filter it))
      :cell-renderer (.setCellRenderer tv (make-table-view-cell-renderer it))
      :header-data (.setHeaderData tv it) ;; String?
      :name (.setName tv (str it))
      :sort-direction (.setSortDirection tv (get-sort-direction it))
      :width (.setWidth tv it)
      :relative (.setWidth tv (.getWidth tv) it))
    tv))


;;http://mail-archives.apache.org/mod_mbox/incubator-pivot-user/200909.mbox/%3C168ef9ac0909080333u7113b048wd5601d87d0c34804@mail.gmail.com%3E
;;> Can I invoke the editor in my code rather than relying on a double click to
;;> invoke it? For example I might have an "Edit" button.
;;
;;Yep - you can call tableView.getRowEditor().edit(tableView,
;;tableView.getSelectedIndex(), columnIndex) to initiate an edit on the
;;selected row at whatever column index you choose.


(def-component-ctor table-view-header [args style]
  (with-component [tvh TableViewHeader]
    (cond-call-it args
      :table-view (.setTableView tvh it)
      :data-renderer (.setDataRenderer tvh it))))

;; buttons

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


;; implementing the render interface

(defn make-cell-renderer
  "Returns a TableView$CellRenderer implemented through the render-f function.
  Calling render-f without an argument should return a component (i.e. a Label)
  which is used to render data cells.
  Upon rendering, render-f is called with a map of arguments:
    :component :value :table-view :column 
    :row-selected? :row-highlighted? :row-disabled?
  and should set the given :component according to its needs, eg:
    (when row-highlighted? (label :self (:component args) :font [\"Arial\" :bold 12]))."
  [render-f]
  (let [component (render-f)]
    (proxy [TableView$CellRenderer] []
      ;; TableView$CellRenderer
      ;;void render(Object value, TableView tableView, TableView.Column column, boolean rowSelected, boolean rowHighlighted, boolean rowDisabled) 
      (render [value, table-view, table-view-column, row-selected?, row-highlighted?, row-disabled?]
              (render-f {:component component
                         :value value
                         :table-view table-view
                         :column table-view-column
                         :row-selected? row-selected?
                         :row-highlighted? row-highlighted?
                         :row-disabled? row-disabled?})
              nil)
      ;; Renderer   
      ;;Dictionary<String,Object> getStyles() Returns the renderer's style dictionary.
      (getStyles [] (.getStyles component)) 
      ;; ConstrainedVisual    
      ;;int getPreferredHeight(int width) Returns the visual's preferred height given the provided width constraint.
      (getPreferredHeight [width] (.getPreferredHeight component width))
      ;;Dimensions getPreferredSize() Returns the visual's unconstrained preferred size.
      (getPreferredSize [] (.getPreferredSize component))
      ;;int getPreferredWidth(int height) Returns the visual's preferred width given the provided height constraint.
      (getPreferredWidth [] (.getPreferredWidth component))
      ;;void setSize(int width, int height) Sets the visual's render size.
      (setSize [width height] (.setSize component width height))
      ;; Visual
      ;;int getHeight() Returns the visual's height.
      (getHeight [] (.getHeight component))
      ;;int getWidth() Returns the visual's width.
      (getWidth [] (.getWidth component))
      ;;void paint(Graphics2D graphics) Paints the visual.
      (paint [graphics] (.paint component graphics)))))

;; implementing the roweditor interface

(defn make-row-editor
  ""
  []
  )

(defn make-list [s]
  (let [l (org.apache.pivot.collections.LinkedList.)]
    (doseq [elem s]
      (.add l elem))
    l))


(comment
  (import (hoeck.pivot testBean))
  (.getName (testBean. {:name 'e :city-code 11 :mobile-phone-number 999884}))
  (def __t  (table-view
             :preferred-size [400 400]
             :row-editor :cell
             :data (make-list (list (testBean. {:name 'a :city-code 16 :mobile-phone-number 123})
                                    (testBean. {:name 'b :city-code 15 :mobile-phone-number 2})
                                    (testBean. {:name 'c :city-code 14 :mobile-phone-number 222223})
                                    (testBean. {:name 'd :city-code 13 :mobile-phone-number 9876})
                                    (testBean. {:name 'e :city-code 11 :mobile-phone-number 999884})))             
             (table-view-column :name 'name :header-data "Name")
             (table-view-column :name 'cityCode 
                                :header-data "Code"
                                :cell-renderer (make-cell-renderer (fn ([] (PushButton.))
                                                                     ([opts] (.setButtonData (:component opts) "Foo")))))
             (table-view-column :name 'mobilePhoneNumber :header-data "Mobile")))
  ;; table-view with header:
  (show-only (window :maximized true
                     (scrollpane :column-header (table-view-header :preferred-size [400 20]
                                                                   :table-view __t)
                                 :view __t)))

  (.getRowEditor (.get (.getColumns __t) 0))
  (.setHeaderData (.get (.getColumns __t) 0) data)
  (seq (.getStyles (table-view-header)))
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
                               )))))



  
  )
