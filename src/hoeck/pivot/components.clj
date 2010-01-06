;   Copyright (c) 2009, Erik Soehnel All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; NOTES:
;; * get-property returns nil if a component-getter throws an exception, this may happen
;;   sometimes when getting preferred size limits or when setting a preferred size outside
;;   of previously set limits
;; * when setting unsupported styles of a component, pivot prints a warning 
;;   (to the *inferior-lisp* buffer)
;; * to disable runtime doc-generation, set the "hoeck.pivot.components.no-documentation" 
;;   property to "true"

(ns hoeck.pivot.components
  (:use clojure.contrib.pprint
	clojure.contrib.prxml
	clojure.contrib.duck-streams
        clojure.contrib.except

	hoeck.pivot.datastructures
        hoeck.pivot.icons)
  (:require [hoeck.pivot.Application :as app]
            [clojure.set :as set]
	    [clojure.xml :as xml])
  (:import (org.apache.pivot.wtk DesktopApplicationContext Application Display
                                 ;; containers
				 Container
                                 Window
                                 Form Form$Section
                                 SplitPane SplitPane$Region
                                 TabPane Accordion BoxPane Border
				 ScrollPane ScrollPane$ScrollBarPolicy Panorama
                                 StackPane Frame Viewport Panel FlowPane CardPane
                                 TablePane TablePane$Column TablePane$Row
                                 MovieView
				 ;; buttons
				 Button Button$Group PushButton RadioButton
				 Checkbox LinkButton Button$State
                                 ;; other components
                                 Component 
                                 Label Slider
                                 TextInput TextArea                                 
                                 FileBrowser FileBrowserSheet
                                 Calendar CalendarButton
                                 Spinner Rollup Expander Sheet ActivityIndicator 
                                 Separator Palette ScrollBar
                                 ImageView Meter Prompt
                                 ;; popups
                                 Dialog Tooltip Alert
                                 ;; tree
                                 TreeView
                                 TreeView$NodeEditor TreeView$NodeRenderer TreeView$NodeCheckState
                                 TreeView$PathComparator TreeView$SelectMode
                                 ;; list
                                 ListButton ListView
                                 ListView$SelectMode ListView$ItemRenderer ListView$ItemEditor
                                 ;; menu
                                 Menu MenuBar MenuBar$Item MenuButton Menu$Item Menu$Section MenuPopup
                                 ;; tables
                                 TableView TableViewHeader
				 TableView$SelectMode TableView$Column
				 TableView$CellRenderer TableView$RowEditor
                                 ;; enums, structs
                                 Orientation SortDirection Insets Point Bounds
				 Dimensions VerticalAlignment HorizontalAlignment
                                 ;; button-action
                                 Action)
           
           (org.apache.pivot.wtk.content TableViewBooleanCellRenderer 
                                         TableViewCellRenderer
                                         TableViewDateCellRenderer
                                         TableViewFileSizeCellRenderer
                                         TableViewImageCellRenderer
                                         TableViewMultiCellRenderer
                                         TableViewNumberCellRenderer
                                         TableViewCellEditor
                                         TableViewRowEditor
                                         ListViewColorRenderer
                                         ListViewItemRenderer
                                         ListViewItemEditor
                                         TreeViewNodeRenderer
                                         TreeViewNodeEditor
					 TreeBranch
					 TreeNode
                                         ButtonData
                                         ListItem)
           (org.apache.pivot.wtk.text.validation Validator)
           (org.apache.pivot.util Filter CalendarDate)
	   (org.apache.pivot.collections Map Dictionary)
	   (java.awt Color Font)
	   (java.net URL)))

;; enum/classes mapping

(def components ;; a list of component classes
     [Border Frame ActivityIndicator ScrollPane Display TabPane
      TextInput StackPane TableView Menu MenuBar Accordion Panorama
      Viewport MenuButton TextArea ListButton Window Calendar
      Spinner Rollup Separator Container PushButton TablePane
      TableViewHeader Checkbox Tooltip Alert Palette ScrollBar
      CalendarButton Expander Panel BoxPane Form Sheet LinkButton
      FileBrowser FlowPane Slider RadioButton ImageView MenuPopup Label
      Meter ListView TreeView MovieView SplitPane Dialog FileBrowserSheet
      Prompt Button CardPane Menu$Item Component])

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

(defn make-insets [[top left bottom right]]
  (Insets. top left bottom right))

(defn get-font-style [keyword]
  (condp = keyword
    nil Font/PLAIN
    :plain Font/PLAIN
    :bold Font/BOLD))

(defn get-color [color-keyword-or-vector]
  (let [c color-keyword-or-vector]
    (cond (keyword? c)
          (throwf "todo: implement color lookup")
          (vector? c)
          (let [[r g b a] c]
            (cond (and r g b a) (Color. r g b a)
                  (and r g b) (Color. r g b)
                  (and r g) (Color. r g)
                  (and r) (Color. r)
                  :else (throw-arg "need at least one non nil argument to Color ctor")))
	  (integer? c) (Color. c)
          (instance? Color c) c
          :else (throw-arg "to get-color: %s" c))))

;; todo: add more style keywords
;; a map of style-keywords to [styleStringKey function]
(def style-setters
     {:padding ["padding" make-insets]
      :vert-align ["verticalAlignment" get-vert-align]
      :vertical-alignment ["verticalAlignment" get-vert-align]
      :horiz-align ["horizontalAlignment" get-horiz-align]
      :horizontal-alignment ["horizontalAlignment" get-horiz-align]
      :spacing ["spacing" identity]
      :pref-size ["preferredSize" (fn [[x y]] (Dimensions. x y))]
      :thickness ["thickness" identity]
      :height ["height" identity]
      :width ["width" identity]
      :focusable ["focusable" identity]
      :button-padding ["buttonPadding" make-insets]
      :font ["font" (fn [[name style size]] (Font. name (get-font-style style) size))]
      :font-size ["fontSize" identity]
      :font-bold ["fontBold" boolean]
      :font-italic ["fontItalic" boolean]
      :fill ["fill" boolean]
      :delimiter ["delimiter" str]
      :show-first-section-heading ["showFirstSectionHeading" boolean]
      :opaque ["opaque" boolean]
      :wrap-text ["wrapText" boolean]
      ;; tablePane
      :show-horizontal-grid-lines ["showHorizontalGridLines" boolean]
      :show-vertical-grid-lines ["showVerticalGridLines" boolean]
      :horizontal-spacing ["horizontalSpacing" int]
      :vertical-spacing ["verticalSpacing" int]
      ;; colors
      :active-tab-color ["activeTabColor" get-color]
      :invalid-background-color ["invalidBackgroundColor" get-color]
      :invalid-color ["invalidColor" get-color]
      :color ["color" get-color]
      :background-color ["backgroundColor" get-color]
      :border-color ["borderColor" get-color]
      :title-bar-color ["titleBarColor" get-color]
      :title-bar-background-color ["titleBarBackgroundColor" get-color]
      :title-bar-border-color ["titleBarBorderColor" get-color]})

(defn set-styles
  "set the style of a component using style-setters to translate requests to pivot."
  [component style-map]
  (let [sd (.getStyles component)]
    (doseq [[k v] style-map]
      (if (keyword? k)
        ;; use style setters
        (let [[key f] (style-setters k)]
          (when (nil? f) (throwf "no setter for style %s" k))
          (.put sd key (f v)))
        ;; use raw pivot classes
        (.put sd k v)))
    component))

(defn set-user-data
  "put the contents of the (clojure-hashmap) user-data-map
  into the userdata dictionary of component."
  [component user-data-map]
  (let [u (.getUserData component)]
    (doseq [[k v] user-data-map]
      (.put u (str k) v))
    u))

(defn get-orientation [keyword]
  (condp = keyword 
    :horiz Orientation/HORIZONTAL
    :vert Orientation/VERTICAL))

(defn get-scrollbar-policy
  "Mapping from keywords to ScrollPane$ScrollBarPolicy enums."
  [key]
  (condp = key
    :always ScrollPane$ScrollBarPolicy/ALWAYS
    :auto ScrollPane$ScrollBarPolicy/AUTO
    :fill ScrollPane$ScrollBarPolicy/FILL
    :fill-to-capacity ScrollPane$ScrollBarPolicy/FILL_TO_CAPACITY
    :never ScrollPane$ScrollBarPolicy/NEVER))

(defn get-table-view-select-mode [keyword]
  (condp = keyword
    :multi TableView$SelectMode/MULTI
    :none TableView$SelectMode/NONE
    :single TableView$SelectMode/SINGLE))

(defn get-sort-direction [keyword]
  (condp = keyword
    :asc SortDirection/ASCENDING
    :desc SortDirection/DESCENDING))

(defn get-button-state [key]
  (condp = key
    :selected Button$State/SELECTED
    :mixed Button$State/MIXED
    :unseleced Button$State/UNSELECTED))

(defn get-button-data
  "Get :icon and :text from a ButtonData instance, or return bd."
  [bd] (if (isa? (type bd) ButtonData)
         {:icon (.getIcon bd) :text (.getText bd)}
         bd))

(defn make-button-data
  "Return a ButtonData object if input is a vector of [icon & [text]],
   a hashmap containing :text, :icon and more keys resulting in a 
  ButtonData object implementing clojure.lang.ILookup.
  otherwise just return the argument."
  ([arg] 
     (cond (vector? arg)
           (let [[icon & [text]] arg]
             (if text 
               (ButtonData. (get-icon icon) (str text))
               (ButtonData. (get-icon icon))))
           (map? arg)
           (proxy
               [ButtonData clojure.lang.ILookup]
               [(when-let [i (:icon arg)] (get-icon i))
                (when-let [t (:text arg)] (str t))]
             (valAt ([k] (get arg k))
                    ([k nf] (get arg k nf))))
           :else arg)))

(defn make-button-action 
  "Given a function, return a org.apache.pivot.wtk.Action calling (f)
  on Action.perform()."
  [f]
  (when f
    (proxy [Action] []
      (perform [] (f)))))

(defn make-list-item
  ;; basically the same as ButtonData
  "Return a ListItem object if input is a vector of [icon & [text]],
   a hashmap containing :text, :icon and more keys resulting in a 
  ButtonData object implementing clojure.lang.ILookup.
  otherwise just return the argument."
  ([arg]
     (cond (vector? arg)
           (let [[icon & [text]] arg]
             (if text 
               (ListItem. (get-icon icon) (str text))
               (ListItem. (get-icon icon))))
           (map? arg)
           (proxy
               [ListItem clojure.lang.ILookup]
               [(when-let [i (:icon arg)] (get-icon i))
                (when-let [t (:text arg)] (str t))]
             (valAt ([k] (get arg k))
                    ([k nf] (get arg k nf))))
           :else arg)))

(defn get-listview-selectmode [key]
  (condp = key
    :multi ListView$SelectMode/MULTI  
    :none ListView$SelectMode/NONE
    :single ListView$SelectMode/SINGLE))

(defn get-bounds
  "Given a org.apache.pivot.wtk.Bounds object, return its values as
  a vector [x y width height].
  Given a clojure vector, return a Bounds object."
  [bounds-or-vector]
  (let [bv bounds-or-vector]
    (if (instance? Bounds bv)
      (vector (.x bv) (.y bv) (.width bv) (.height bv))
      (let [[x y w h] bv] (Bounds. x y w h)))))

;; interfaces

(defn make-validator
  "Return an org.apache.pivot.wtk.text.validation.Validator.
  f must be a function of at least one arg, which is called with
  a String and should return logical true if the string is valid."
  [f]
  (proxy [Validator] []
    (isValid [text] (if (f text) true false))))

(defn make-filter
  "Return a org.apache.pivot.util.Filter with the function f performing
  the boolean include(Object item) method. f should return logical true for
  item to be included."
  [f]
  (proxy [Filter] []
    (include [item] (if (f item) true false))))

;; table-view renderer

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

(defn make-table-view-cell-renderer
  "Return a TableView$CellRenderer, defaults (= arg nil) to
  one that renders strings.
  arg may be a function implementing the cell-renderer, see make-cell-renderer"
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
    ;; assume arg to be a function
    (make-cell-renderer arg)))

(defn make-list-view-renderer [arg]
  (if (keyword? arg)
    (condp = arg
      :default (ListViewItemRenderer.)
      :color (ListViewColorRenderer.))))

(defn make-list-view-editor [arg]
  (if (keyword? arg)
    (condp = arg
      :default (ListViewItemEditor.))
    (throwf "todo")))

;; table-view roweditors

(defn make-table-view-editor
  "return a TableView$RowEditor either a default one (arg is :row or :cell)
   or a custom one, (see hoeck.pivot.content)."
  [arg]
  (cond (keyword? arg) (condp = arg
                         :row (TableViewRowEditor.)
                         :cell (TableViewCellEditor.))
        (fn? arg) (hoeck.pivot.content/table-view-editor arg)
        :else arg))

;; tree view helpers

(defn get-tree-view-select-mode [key]
  (condp = key
    :single TreeView$SelectMode/SINGLE
    :multi TreeView$SelectMode/MULTI
    :none TreeView$SelectMode/NONE))

(defn make-tree-view-node-editor
  "Return a TreeView$NodeEditor implemented by function f.
  f is called with the edit params when the editors edit method
  is invoked. Edit params are: this, treeview and a Sequence$Tree$Path.
  Other method invocations are handed over to f
  with a single keyword (:cancel, :is-editing and :save)"
  [f]
  (proxy [TreeView$NodeEditor] []
    (edit [treeview, path]
          (f this treeview path))
    (cancel [] (f this :cancel))
    (isEditing [] (f this :is-editing))
    (save [] (f this :save))))

(defn get-tree-view-node-editor
  "either :default or an edit-function.
  see make-tree-view-node-editor."
  [arg]
  (if (= :default arg)
    (TreeViewNodeEditor.)
    (make-tree-view-node-editor arg)))

(defn make-tree-view-node-renderer [f]
  ;; TODO
  (proxy [TreeView$NodeEditor] []))

(defn get-tree-view-node-renderer [arg]
  (cond
    (= :default arg) (TreeViewNodeRenderer.)
    (isa? arg TreeView$NodeRenderer) arg
    :else (make-tree-view-node-renderer arg)))

;; tools for component constructors

(defn parse-component-args
  "Read arglist, assume a sequence of :key value pairs, pack them into a hashmap.
  Leave the remaining args in the seq and return [hashmap rest].
  If the first argument is a hashmap, return it and the remaing arguments."
  [arglist]
  (if (map? (first arglist))
    [(first arglist) (rest arglist)]
    (loop [a arglist
           args {}]
      (if (seq a)
        (let [[k v & more] a]
          (if (keyword? k)
            (recur more (assoc args k v))
            [args a]))
        [args a]))))

;;(parse-component-args [:a 1 :b 2 (BoxPane.) (Border.) (Border.)])

(defmacro defcomponent
  "Define a component constructor. Arg-keys is a vector of symbols naming
  keys from the arguments parsed with parse-component-ctor-args."
  [name arg-vec & body]
  `(defn ~name [& args#]
     (let [~arg-vec (parse-component-args args#)]
       ~@body)))

(defmacro with-component
  "For use within the defcomponent macro. Requires `args'
  to be bound to the keyargs map. Executes body with in a component
  bound to component-name.
  Component is eiter instanciated using the given classnames default ctor
  or by looking up :self in args.
  Classname may also be a list, in which case the lists rest is used as 
  arguments to the ctor."
  [[component-name classname] & body]
  `(let [~component-name (or (:self ~'args) (new ~@(if (seq? classname)
						     classname
						     (list classname))))
         ~'args (dissoc ~'args :self)]
     (set-properties ~component-name ~'args)
     ~@body
     ~component-name))

;; Component Properties

(def property-definition-map {})

(defmacro defproperties
  "Define properties of an object and setter and getter expressions
     :key setter getter docstring
  If there is no getter for a value, use nil, if there is no setter fo
  a value, use (throwf \"why?\") which throws an exception when someone tries
  to use it."
  [type [varname] & properties]
  `(alter-var-root (resolve 'property-definition-map) assoc ~type
		   ~(into {} (map (fn [[key setter getter doc]]
				    [key `{:setter (fn [~varname ~'it] ~setter)
					   :getter (fn [~varname] ~getter)
					   :doc ~doc}])
				  (partition 4 properties)))))

(defn get-all-property-defs
  "Return a map of property definitions for a given object o."
  [o]
  (let [types (conj (supers (type o)) (type o))]
    (apply merge (map property-definition-map types))))

(defn set-properties
  "Set properties of object known as name to values found in the prop hashmap.
  Return the object. Throw an Exception if a key is not a property of o."
  ([o props]
     (let [p (get-all-property-defs o)
	   get-setter #(get-in p [% :setter])]
       (doseq [[k v] props]
	 (if-let [s (get-setter k)]
	   (s o v)
	   (throwf "Unknown property %s for object %s" k o)))
       o)))

(defn set-property
  "Set a single property of a component and return the component."
  [o key value]
  (set-properties o {key value})
  o)

(defn get-properties
  "Return all properties of an object or only the ones given in property-keys.
  If an Exception occurs in a getter-method call, ignore it and return nil for
  that value."
  [o & property-keys]
  (if (nil? o)
    (throwf "o may not be nil")
    (let [p (get-all-property-defs o)
          pk (if (empty? property-keys)
               (constantly true)
               (set property-keys) )]
      (into {} (map (fn [[k {:keys [getter]}]]
                      (when (and (pk k) getter) [k (try (getter o) (catch Exception e nil))]))
                    p)))))

(defn get-property
  "return the value of the single property given with key."
  [o key]
  (get (get-properties o key) key))

(defn doc-properties
  "return a docstring made up of the objects properties of the form: 
  :prop-key   .. documentation."
  [o]
  (cl-format nil "~:{~22<~a~; ..~> ~a~%~}"
             (let [p (get-all-property-defs o)]
               (map vector
                    (keys p)
                    (map #(-> % val :doc) p)))))

(defmacro set-documentation
  "generate extended docstrings from property docs, generate no documentation at all when
  the \"hoeck.pivot.components.no-documentation\" property is set to \"true\"."
  [var o & arglist-spec]  
  (if (= (System/getProperty "hoeck.pivot.components.no-documentation") "true")
    nil
    `(let [o# ~o
           a# '~arglist-spec]
       (alter-meta! (resolve '~var)
                    assoc
                    :doc (str "Returns a new or existing (given with :self) "
                              (type o#) " with the given properties set" \newline
                              (doc-properties o#))
                    :arglists (if (= [:keys] a#)
                                (list (vec (keys (get-properties o#))))
                                (if (= (first a#) :keys)
                                  (vec (concat (keys (get-properties o#)) (next a#)))
                                  (vec a#)))))))

;; helper macros

(defmacro when-it
  "Anaphoric when using ìt'."
  [expr & body]
  `(when-let [~'it ~expr] ~@body))

(defmacro set-relative-size
  "generate a setter for optional relative-sized components:
  method, o and arg must all be symbols as they will be evaluated more than once.
  arg: [100] -> (method o 100 true)
  arg: 100   -> (method o 100 false)"
  [method o arg]
  `(if (vector? ~arg)
     (~method ~o (nth ~arg 0) true);; relative
     (~method ~o ~arg false)))

(defmacro get-relative-size
 "expands into a getter-expression for optionally relative-sized components
  method, o must all be symbols as they will be evaluated more than once."
  [method o]
  `(if (.isRelative ~o)
     (vector (~method ~o))
     (~method ~o)))

;; the definitions

(defn set-component-width [c sizedef]
  (let [[min pref max] (if (vector? sizedef) sizedef [* sizedef *])]
    (when (not= min *) (.setMinimumPreferredWidth c min))
    (when (not= pref *) (.setPreferredWidth c pref))
    (when (not= max *) (.setMaximumPreferredWidth c max))))

(defn set-component-height [c sizedef]
  (let [[min pref max] (if (vector? sizedef) sizedef [* sizedef *])]
    (when (not= min *) (.setMinimumPreferredHeight c min))
    (when (not= pref *) (.setPreferredHeight c pref))
    (when (not= max *) (.setMaximumPreferredHeight c max))))

(defproperties Component [c]
  :enabled (.setEnabled c it) (.isEnabled c) "Enabled status."
  :height (.setHeight c it) (.getHeight c) "Height"
  :width (.setWidth c it) (.getWidth c) "Width"
  :visible (.setVisible c (boolean it)) (.isVisible c) "Visible flag"

  :parent nil (.getParent c) "The components parent, read-only."

  :pr-w
  (set-component-width c it)  
  [(.getMinimumPreferredWidth c) (.getPreferredWidth c) (.getMaximumPreferredWidth c)]
  "shorthand for :preferred-width"

  :preferred-width
  (set-component-width c it)  
  [(.getMinimumPreferredWidth c) (.getPreferredWidth c) (.getMaximumPreferredWidth c)]
  "Set the preferred width of the Component. Can be a single number (= pref) or a vector of [min pref max],
  where min, pref, max are numbers or `*' where the latter indicates leave-value-as-is."
  
  :pr-h
  (set-component-height c it)
  [(.getMinimumPreferredHeight c) (.getPreferredHeight c) (.getMaximumPreferredHeight c)]
  "shorthand for :preferred-height"

  :preferred-height
  (set-component-height c it)  
  [(.getMinimumPreferredHeight c) (.getPreferredHeight c) (.getMaximumPreferredHeight c)]
  "Set the preferred height of the Component. Can be a single number (= pref) or a vector of [min pref max],
  where min, pref, max are numbers or `*' where the latter indicates leave-value-as-is."

  :preferred-size
  (let [[x y] it] (.setPreferredSize c x y))
  [(.getPreferredWidth c) (.getPreferredHeight c)]
  "Preferred size of the component, a vector of [width height]"

  :tooltip-text
  (.setTooltipText c (str it))
  (.getTooltipText c)
  "a string"

  :styles (set-styles c it) (dictionary->hashmap-str (.getStyles c)) "A map of styles for the component."

  :location
  (let [[x y] it] (.setLocation c (Point. x y)))
  (let [loc (.getLocation c)] [(.x loc) (.y loc)])
  "A [x,y] value containing the component's horizontal and vertical position relative to the origin of the parent container."

  :user 
  (set-user-data c it)
  (dictionary->hashmap (.getUserData c))
  "Userdata of Component, a clojure hashmap."
  
  :user-name
  (set-user-data c {:name it})
  (.get (.getUserData c) ":name")
  "The userdata :name content, shorthand for (set-property c :user {:name 'name})"
  
  :user-tags
  (set-user-data c {:tags (cond (keyword? it) (conj (or (.get (.getUserData c) ":tags") #{}) it)
                                (nil? it) #{}
                                (set? it) (reduce conj (or (.get (.getUserData c) ":tags") #{}) it)
                                :else (throwf "only keywords, nil or sets are accepted as user-tags"))})
  (.get (.getUserData c) ":tags")
  "user-tags, reside in :user under :tags key, may be a set of keys or a single key.
  Returns always a set. Setting is always ADDs a key to the user-tags.
  nil removes all keys.")

(defproperties Container [c]
  :components 
  (do (.removeAll c)
      (if (or (seq? it) (vector? it))
	(doseq [comp it] (.add c comp))
	(.add c it)))
  (seq c)
  "a list of components of this container, setting sets it to a single component or a seq/vector of components")

;; containers

(defproperties Window [w]
  :content (.setContent w it) (.getContent w) "the component the window encloses"
  :owner (.setOwner w it) (.getOwner w) "the owner (another Window or Display) of this window"
  :maximized (.setMaximized w it) (.isMaximized w) "when true, maximize window over the whole display"
  :visible (.setVisible w it) (.isVisible w) "hide window when false"
  :title (.setTitle w (str it)) (.getTitle w) "the windows title"
  :icon (when-it (get-icon it) (.setIcon w it)) (.getIcon w) "the windows icon, a keyword or an Image object."
  :auxilliary nil (.isAuxilliary w) "The window's auxilliary flag. Auxilliary windows can't become active and can only own other auxilliary windows. Not settable, only when constructing the window.")

(defcomponent window [args [component]]
  (with-component [w (Window (:auxilliary args))]
    (when-it component (.setContent w it))))

(defcomponent sheet [args [component]]
  (with-component [s Sheet]
    (.setContent s component)))

(defproperties Frame [fr]
  :menu-bar (.setMenuBar fr it) (.getMenuBar fr) "the frames menu-bar.")

(defcomponent frame [args [component]]
  (with-component [fr Frame]
    (when component (.setContent fr component))))

(set-documentation window (Window.) :keys component)
(set-documentation sheet (Sheet.) :keys component)
(set-documentation frame (Frame.) :keys component)


(defproperties BoxPane [b]
  :orientation (.setOrientation b (get-orientation it)) (.getOrientation b)
  "The orientation of the components of the boxpane: :vert :vertical or :horiz :horizontal.")

(defcomponent boxpane [args components]
  (with-component [b BoxPane]
    (doseq [c components] (.add b c))))

(set-documentation boxpane (BoxPane.) :keys & components)


(defproperties Border [b]
  :content (.setContent b it) (.getContent b) "the component to draw a border around"
  :title (.setTitle b (str it)) (.getTitle b) "The borders title.")

(defcomponent border [args [component]]
  (with-component [b Border]
    (when-it component (.setContent b it))))

(set-documentation border (Border.) :keys component)

(defproperties Panel [p])
(defcomponent panel [args components]
  (with-component [p Panel]
    (doseq [c components] (.add p c))))

(defproperties Accordion [a]
  :selected-index (.setSelectedIndex a it) (.getSelectedIndex a) "The currently selected accordion-pane.")

(defcomponent accordion [args accordion-panels]
  (with-component [a Accordion]
    (doseq [acc-panel-f accordion-panels]
      (acc-panel-f a))))

;; todo: handle :self in accordion-panel
(defcomponent accordion-panel [args [component]];; meta component for accordion panels
  (fn [accordion]
    (.add (.getPanels accordion) component)
    (when-it (:label args) (Accordion/setLabel component (str it)))
    (when-it (:icon args) (when-it (get-icon it) (Accordion/setIcon component it)))
    component))

(set-documentation accordion (Accordion.) :keys & accordion-panels)
(alter-meta! #'accordion-panel assoc :doc
	     (str "create a accordion-panel closure to add to hold a single component shown in an accordion." \newline
		  "  :label .. the label of the accordion panel" \newline
		  "  :icon  .. if set appears in front of the label.")
	     :arglists '([:label :icon :self component]))


(defproperties CardPane [c]
  :selected-index
  (.setSelectedIndex
   c (cond (integer? it) it
           (instance? Component it) (.indexOf c it)
           :else (throwf "only integer of components allowed, not  %s" it)))
  (.getSelectedIndex c)
  "the index of the selected (== shown) component. Setable with an integer or a component.")


(defcomponent cardpane [args components]
  (with-component [p CardPane]
    (doseq [c components] (.add p c))))

(set-documentation cardpane (CardPane.) :keys & components)


(defproperties StackPane [s])
(defcomponent stackpane [args components]
  (with-component [s StackPane]
    (doseq [c components] (.add s c))))
(set-documentation stackpane (StackPane.) :keys & components)

(defproperties Expander [e]
  :title (.setTitle e (str it)) (.getTitle e) "The expanders title."
  :collapsible (.setCollapsible e (boolean it)) (.isCollapsible e) "Collapsile flag."
  :expanded (.setExpanded e (boolean it)) (.isExpanded e) "Expand status."
  :content (.setContent e it) (.getContent e) "The component inside the expander.")

(defcomponent expander [args component]
  (with-component [e Expander]
    (when-not (empty? component)
      (set-property e :content (first component)))))

(set-documentation expander (Expander.) :keys content)


(defproperties Rollup [r]
  :content (.setContent r it) (.getContent r) "The content component."
  :heading (.setHeading r it ) (.getHeading r) "The component displaying the heading." 
  :collapsible (.setCollapsible r (boolean it)) (.isCollapsible r) "Collapsible flag."
  :expanded (.setExpanded r (boolean it)) (.isExpanded r) "Expanded flag.")

(defcomponent rollup [args [heading content]]
  (with-component [r Rollup]
    (set-property r :heading heading)
    (set-property r :content content)))

(set-documentation rollup (Rollup.) :keys heading content)

;; imageview

(defproperties ImageView [i]
  :image (.setImage i (get-icon it)) (.getImage i) "The image to display.")

(defcomponent imageview [args]
  (with-component [i ImageView]))

(set-documentation imageview (ImageView.) :keys)

;; separator

(defproperties Separator [s]
  :heading (.setHeading s (str it)) (.getHeading s) "Heading for the separator")

(defcomponent separator [args]
  (with-component [s Separator]))

(set-documentation separator (Separator.) :keys)

;; forms

(defcomponent form [args form-sections]
  (with-component [fm Form]
    (doseq [add-section-f form-sections]
      (add-section-f fm))))

(defcomponent form-section [args form-component-functions-or-components]
  ;; when plain component is given, then setting the label is not possible
  ;; when a (form-component ...) is given, label setting works
  (fn [form]
    (let [sec (or (:self args) (Form$Section.))]
      (.add (.getSections form) sec)
      (doseq [add-form-component-f form-component-functions-or-components]
        (if (fn? add-form-component-f)
          (add-form-component-f sec)
          (.add sec add-form-component-f)))
      (when-it (:heading args) (.setHeading sec (str it))))))

;; todo: handle :self
(defcomponent form-component [args [component]]
  (fn [section]
    (.add section component)
    (when-it (:label args) (Form/setLabel component (str it)))
    component))

(set-documentation form (Form.) :keys & form-sections)
(alter-meta! #'form-section assoc :doc
	     (str "returns a form section from mutliple form.components" \newline
		  "  :heading .. an optional heading for the section")
	     :arglists '([:heading & form-components]))
(alter-meta! #'form-component assoc :doc
	     (str "returns a form component from a component" \newline
		  "  :label .. an optional lable for the component")
	     :arglists '([:label component]))

;; tabs

(defproperties TabPane [t]
  :corner (.setCorner t it) (.getCorner t) "set/get a corner component"
  :selected-index (.setSelectedIndex t it) (.getSelectedIndex t)
  "the currently selected tab index"
  :tabs
  (let [ts (.getTabs t)]
    (.remove ts 0 (.getLength ts))
    (doseq [add-tab-f it]
      (add-tab-f ts)))
  (.getTabs t)
  "Set the tabs (see `tabpane-panel').")

(defcomponent tabpane [args tabpane-panel-functions]
  (with-component [t TabPane]
    (let [tab-sequence (.getTabs t)]
      (doseq [add-tab-f tabpane-panel-functions]
        (add-tab-f tab-sequence)))))

(defcomponent tabpane-panel [args [component]]
  (fn [tab-sequence]
    (.add tab-sequence component)
    (when-it (:closeable args) (TabPane/setCloseable component it))
    (when-it (:label args) (TabPane/setLabel component it))
    (when-it (:icon args) (when-it (get-icon it) (TabPane/setIcon component it)))))

(set-documentation tabpane (TabPane.) :keys & tabpane-panels)
(alter-meta! #'tabpane-panel assoc :doc
	     (str "returns a tabpane-panel from a component" \newline
		  "  :closeable .. user may close the tabpane" \newline
		  "  :label     .. tabpane label text" \newline
		  "  :icon      .. tabpane icon (a :key to get-icon)")
	     :arglists '([:closeable :label :icon component]))

(defproperties SplitPane [s]
  :locked (.setLocked s it) (.isLocked s) "when true, don't allow the user to move the splitbar"
  :orientation (.setOrientation s (get-orientation it)) (.getOrientation s) ":vert or :horiz, splitbar orientation"

  :primary-region 
  (.setPrimaryRegion s (condp = it
                         :top-left SplitPane$Region/TOP_LEFT
                         :bottom-right SplitPane$Region/BOTTOM_RIGHT))
  (.getPrimaryRegion s)
  ":top-left or :bottom-right, determines the primary region of the splitpane, eg. the one which is shown at 1.0 split ratio"

  :split-ratio (.setSplitRatio s it) (.getSplitRatio s) "a float determining the ratio between the two nested components"
  ;; components
  :bottom (.setBottom s it) (.getBottom s) "the bottom component in a :horizontal splitpane"
  :bottom-right (.setBottomRight s it) (.getBottomRight s) "the bottom or right component"
  :left (.setLeft s it) (.getLeft s) "the left component in a :vertical splitpane"
  :right (.setRight s it) (.getRight s) "the right component in a :vertical splitpane"
  :top (.setTop s it) (.getLeft s) "the top component in a :horizontal splitpane"
  :top-left (.setTopLeft s it) (.getTopLeft s) "the top or left component")

(defcomponent splitpane [args]
  (with-component [sp SplitPane]))

(set-documentation splitpane (SplitPane.) :keys)

;; views

(defproperties Viewport [v]
  :consume-repaint (.setConsumeRepaint v it) (.isConsumeRepaint v)
  "the consumeRepaint flag, which controls whether the viewport will propagate repaints to its parent or consume them."  
  :scroll-left (.setScrollLeft v it) (.getScrollLeft v) "set or get horizontal scroll-amount"
  :scroll-top (.setScrollTop v it) (.getScrollTop v) "set or get horizontal scroll-amount"
  :view (.setView v it) (.getView v) "set/get the view component (the main one to scroll around)")

(defproperties ScrollPane [s]
  :horiz-scrollbar-policy 
  (.setHorizontalScrollBarPolicy s (get-scrollbar-policy it))
  (.getHorizontalScrollBarPolicy s)
  "one of :always :auto :fill :fill-to-capacity :never"
  
  :vert-scrollbar-policy 
  (.setVerticalScrollBarPolicy s (get-scrollbar-policy it))
  (.getVerticalScrollBarPolicy s)
  "one of :always :auto :fill :fill-to-capacity :never"
  
  ;; components
  :corner (.setCorner s it) (.getCorner s) "the corner component"
  :row-header (.setRowHeader s it) (.getRowHeader s) "the row header component"
  :column-header (.setColumnHeader s it) (.getColumnHeader s) "the column header component")0

(defcomponent scrollpane [args components]
  (with-component [s ScrollPane]
    (doseq [c components] (.add s c))))

(defcomponent panorama [args components]
  (with-component [p Panorama]
    (doseq [c components] (.add p c))))

(set-documentation scrollpane (ScrollPane.) :keys)
(set-documentation panorama (Panorama.) :keys)

;; components

(defproperties Slider [s]
  :range (let [[min max] it] (.setRange s min max)) [(.getStart s) (.getEnd s)] "the range [min max] of the slider"
  :value (.setValue s it) (.getValue s) "the value of the slider")

(defcomponent slider [args] ;; why is a slider a container?
  (with-component [sl Slider]
    (doseq [c components] (.add sl c))))

(set-documentation slider (Slider.) :keys)

;; text

(defproperties Label [l]
  :text (.setText l it) (.getText l) "the text")

(defcomponent label [args text]
  (with-component [la Label]
    (when-not (empty? text)
      (set-property la :text (apply str text)))))

(defproperties TextInput [t]
  :max-length (.setMaximumLength t it) (.getMaximumLength t) "max length in characters"
  :password (.setPassword t it) (.isPassword t) "hide letters when its a password field"
  :prompt (.setPrompt t (str it)) (.getPrompt t) "the text appearing in an empty textinput"
  :selection (let [[s e] it] (.setSelection t s e)) (.getSelection t) "selected text: [start end]"
  :text (.setText t (str it)) (.getText t) "the textinputs content"
  :text-key (.setTextKey t (str it)) (.getTextKey t) "string key for data-binding, calls str on its arg when setting."
  :text-node (.setTextNode t it) (.getTextNode t) "a TextNode"
  :text-size (.setTextSize t it) (.getTextSize t) "length of the inputs text"
  :validator (.setValidator t (make-validator it)) (.getValidator t) "a function implementing the Validator interface")

(defcomponent text-input [args]
  (with-component [ti TextInput]))

(defproperties TextArea [t]
  :editable (.setEditable t it) (.isEditable t) "editable flag"
  :selection (let [[s e] it] (.setSelection t s e)) (.getSelection t) "selected text: [start end]"
  :text (.setText t (str it)) (.getText t) "the current text"
  :text-key (.setTextKey t (str it)) (.getTextKey t) "a string key for data-binding")

(defcomponent text-area [args]
  (with-component [ta TextArea]))

(set-documentation label (Label.) :keys & label-text)
(set-documentation text-input (TextInput.) :keys)
(set-documentation text-area (TextArea.) :keys)


;; tables

(defproperties TableView [t]
  :disabled-filter (.setDisabledRowFilter t (make-filter it)) (.getDisabledRowFilter t) "a predicate-function implementing a Filter"
  :editor (.setRowEditor t (make-table-view-editor it)) (.getRowEditor t) "a funciton implenting editor, see make-table-view-editor"

  :selected-index 
  (if (number? it) 
    (.setSelectedIndex t it)
    (let [[s e] it] (.setSelectedRange t s e))) ;; TODO: add support for ranges [[s0 e0] [s1 e1] ..]
  (.getSelectedRows t)
  "sets a selected-row or a range of rows [start end]"
  
  :select-mode (.setSelectMode t (get-table-view-select-mode it)) (.getSelectMode t)
  "the row select mode, one of: :multi :single :none"
  
  :data (.setTableData t it) (.getTableData t) "a List of Dictionaries of strings to any value or a list of javabeans"
  :cols 
  (let [cs (.getColumns t)]
    (when (< 0 (.getLength cs)) (.remove cs 0 (.getLength cs)))
    (doseq [c it] (.add cs c)))
  (.getColumns t)
  "The table-views columns. A list or vector of table-view-columns.")

(defcomponent table-view [args columns] ;; actually, components are TableView$Columns
  (with-component [t TableView]
    (let [col-sequence (.getColumns t)]
      (doseq [c columns]
        (.add col-sequence c)))))

(defproperties TableView$Column [t]
  :filter (.setFilter t (make-filter it)) (.getFilter t) "a predicate"

  :cell-renderer (.setCellRenderer t (make-table-view-cell-renderer it)) (.getCellRenderer t)
  "one of :boolean :string :date :filesize :image :multi :number, 
  or a custom one (function) see make-table-view-cell-renderer"
  
  :header-data (.setHeaderData t it) (.getHeaderData t) "the displayed column name (a string)"
  :name (.setName t (if (or (symbol? it) (keyword? it)) (name it) (str it))) (.getName t)
  "the name of the key where the data is, e.g. a keyword. The cell renderer
  uses it to get the value to render. Uses name for symbols/keywords to get the
  string."

  :sort-direction (.setSortDirection t (get-sort-direction it)) (.getSortDirection t) ":asc or :desc (for ascending or descending sort order)"
  :width (set-relative-size .setWidth t it) (get-relative-size .getWidth t) "the column width, [width] to set a relative width")

(defcomponent table-view-column [args] ;; not a component, but an object with properties
  (let [t (or (:self args) (TableView$Column.))]
    (set-properties t (merge {:cell-renderer (hoeck.pivot.content/label-cell-renderer)} ;; default renderer
                             (dissoc args :self)))
    t))

(set-documentation table-view (TableView.) :keys table-view-columns)
(set-documentation table-view-column (TableView$Column.) :keys)

;;http://mail-archives.apache.org/mod_mbox/incubator-pivot-user/200909.mbox/%3C168ef9ac0909080333u7113b048wd5601d87d0c34804@mail.gmail.com%3E
;;> Can I invoke the editor in my code rather than relying on a double click to
;;> invoke it? For example I might have an "Edit" button.
;;
;;Yep - you can call tableView.getRowEditor().edit(tableView,
;;tableView.getSelectedIndex(), columnIndex) to initiate an edit on the
;;selected row at whatever column index you choose.

(defproperties TableViewHeader [t]
  :table-view (.setTableView t it) (.getTableView t) "the headers table-view its associated with"

  :data-renderer (.setDataRenderer t it) (.getDataRenderer t) 
  "the header-data renderer (a function which calls the component which
  displays header data, eg. a simple label or a button")

(defcomponent table-view-header [args]
  (with-component [t TableViewHeader]))

(set-documentation table-view-header (TableViewHeader.) :keys)

;; buttons

(defproperties Button [b]
  :action (.setAction b (make-button-action it)) (.getAction b)
  "an Action (a function without args, see make-button-action)"
  :group (.setGroup b (if (isa? (type it) Button$Group) it (str it))) (.getGroup b)
  "the button group, either sth stringable or a Button$Group"
  
  :data
  (.setButtonData b (make-button-data it))
  (get-button-data (.getButtonData b))
  "the buttons text (string) or [icon] or [icon, text]."
  :selected (.setSelected b it) (.isSelected b) "a flag"
  :selected-key (.setSelectedKey b (str it)) (.getSelectedKey b) "key for data binding"
  :state (.setState b (get-button-state it)) (.getState b)
  "button state, one of: :selected :mixed :unselected"
  
  :toggle-state (.setToggleButton b it) (.isToggleButton b) "toggle-state flag"
  :tri-state (.setTriState b it) (.isTriState b) "tri-state flag"
  :renderer (.setDataRenderer b it) (.getDataRenderer b)
  "the renderer for :data")

(defcomponent push-button [args]
  (with-component [pb PushButton]))

(defcomponent radio-button [args]
  (with-component [rb RadioButton]))

(defcomponent checkbox [args]
  (with-component [cb Checkbox]))

(defcomponent link-button [args]
  (with-component [l LinkButton]))

(defproperties ListButton [l]
  :disabled-filter (.setDisabledItemFilter l (make-filter it)) (.getDisabledItemFilter l) "a predicate filtering items"
  :list-data (.setListData l it) (.getListData l) "the listbuttons data, a List"
  :selected-item (.setSelectedItem l it) (.getSelectedItem l) "set an item to select/get the selected item"
  :selected-item-key (.setSelectedItemKey l (str it)) (.getSelectedItemKey l) "a key (string) for databinding"
  :item-renderer
  (.setItemRenderer l (make-list-view-renderer it))
  (.getItemRenderer l)
  ":default or color, or a function, see make-list-view-renderer")

(defcomponent list-button [args]
  (with-component [l ListButton]))

(set-documentation push-button (PushButton.) :keys)
(set-documentation radio-button (RadioButton.) :keys)
(set-documentation checkbox (Checkbox.) :keys)
(set-documentation link-button (LinkButton.) :keys)
(set-documentation list-button (ListButton.) :keys)

(defproperties CalendarButton [c]
  :locale (.setLocale c it) (.getLocale c) "the java.util.Locale the calendar uses"

  :selected-date (if (vector? it)
                   (let [[y m d] it] (.setSelectedDate c (CalendarDate. y m d)))
                   (.setSelectedDate c it))
  (.getSelectedDate c)
  "a pivot.util.CalendarDate or a vector: [year month day]"
  
  :selected-date-key (.setSelectedDateKey c (str it)) (.getSelectedDateKey c) "string key for data binding")

(defcomponent calendar-button [args]
  (with-component [c CalendarButton]))

(set-documentation calendar-button (CalendarButton.) :keys)


;; spinner

(defproperties Spinner [s]
  :selected-index (.setSelectedIndex s it) (.getSelectedIndex s)
  "Currently selected index."
  :selected-item (.setSelectedItem s it) (.getSelectedItem s)
  "Currently selected item."
  :item-renderer (.setItemRenderer s it) (.getItemRenderer s)
  "The item renderer used."
  :selected-item-key (.setSelectedItemKey s (str it)) (.getSelectedItemKey s)
  "The key for data binding."
  :data (.setSpinnerData s it) (.getSpinnerData s)
  "Spinner data, a list."
  :circular (.setCircular s (boolean it)) (.isCircular s)
  "Circular spinning flag.")

(defcomponent spinner [args]
  (with-component [s Spinner]))

(set-documentation spinner (Spinner.) :keys)

;; listview

(defproperties ListView [l]
  :selected 
  (if (vector? it)
    (let [[s e] it] (.setSelectedRange l s e))
    (.setSelectedRange l it))
  (.getSelectedRanges l)
  "the selection, either a single number or a vector: [start end]"

  :selectmode (.setSelectMode l (get-listview-selectmode it)) (.getSelectMode l) ":none :single or :multiple items to select at once."  
  :checkmarks (.setCheckmarksEnabled l it) (.getCheckmarksEnabled l) "checkmarks flag"  
  :disabled-filter (.setDisabledItemFilter l (make-filter it)) (.getDisabledItemFilter l) "disabled item filter, a predicate"
  :item-editor (.setItemEditor l (make-list-view-editor it)) (.getItemEditor l) ":default or a function implementing the ListView$Editor interface"
  :item-renderer (.setItemRenderer l (make-list-view-renderer it)) (.getItemRenderer l) ":default or color, or a function, see make-list-view-renderer"
  :data (.setListData l it) (.getListData l) "list data, probably a dictionary"
  :selected-item-key (.setSelectedItemKey l (str it)) (.getSelectedItemKey l) "string key to bind selected item to"
  :selected-items-key (.setSelectedItemsKey l (str it)) (.getSelectedItemsKey l) "string key to bind selected item to")

(defcomponent listview [args]
  (with-component [l ListView]))

(set-documentation listview (ListView.) :keys)

;; treeview

(defproperties TreeViewNodeRenderer [t]
  :icon-height 
  (.setIconHeight t (if (= :default it) TreeViewNodeRenderer/DEFAULT_ICON_HEIGHT it))
  (.getIconHeight t)
  "the icon height (an int or :default)"
  
  :icon-width 
  (.setIconWidth t (if (= :default it) TreeViewNodeRenderer/DEFAULT_ICON_WIDTH it))
  (.getIconWidth t)
  "the icon width (an int or :default)"

  :show-icon (.setShowIcon t it) (.getShowIcon t) "show icon flag")

(defproperties TreeView [t]
  :checkmarks (.setCheckmarksEnabled t it) (.getCheckmarksEnabled t) "checkmarks flag"
  :disbled-filter (.setDisabledNodeFilter t (make-filter it)) (.getDisabledNodeFilter t) "A filter predicate for nodes."
  :node-editor (.setNodeEditor t (get-tree-view-node-editor it)) (.getNodeEditor t) ":default or a editor function, see make-tree-view-node-editor"
  :node-renderer (.setNodeRenderer t (get-tree-view-node-renderer it)) (.getNodeRenderer t) ":default, a TreeView$NodeRenderer or a function implementing it"
  :select-mode (.setSelectMode t (get-tree-view-select-mode it)) (.getSelectMode t) ":multi :single or :none"
  :data (.setTreeData t it) (.getTreeData t) "tree data, a list of values, either plain strings or treeviews for the default renderer")

(defcomponent tree-view [args]
  (with-component [t TreeView]))

(defproperties TreeNode [t]
  :icon (when-it (get-icon it) (.setIcon t it)) (.getIcon t) "a node icon"
  :text (.setText t (str it)) (.getText t) "node label/text")

(defcomponent tree-node [args]
  (with-component [t TreeNode]))

(defproperties TreeBranch [b]
  :nodes
  (if (or (seq? it) (vector? it)) 
    (doseq [n it] (.add b n))
    (.add b it))
  (seq b)
  "a seq or vector of nodes; setting adds them, reading shows them"

  :comparator (.setComparator b it) (.getComparator b)
  "comparator to sort the nodes in this branch (clojure fns implement the comparator interface)"
  
  :expanded-icon (when-it (get-icon it) (.setExpandedIcon b it)) (.getExpandedIcon b) "expanded icon"
  )

(defcomponent tree-branch [args tree-nodes]
  (with-component [t TreeBranch]
    (doseq [n tree-nodes] (.add t n))))

(set-documentation tree-view (TreeView.) :keys)
(set-documentation tree-node (TreeNode.) :keys)
(set-documentation tree-branch (TreeBranch.) :keys & tree-nodes)


;; table pane

(defproperties TablePane$Column [c]
  :width (set-relative-size .setWidth c it) (get-relative-size .getWidth c) "width, [10] means a relative width."
  :highlighted (.setHighlighted c (boolean it)) (.isHighlighted c) "highlighted flag"
  :visible (.setVisible c (boolean it)) (.isVisible c))

(defcomponent table-pane-column [args]
  (with-component [c TablePane$Column]))

(defn- table-pane-add-rows
  "Set rows of a table-pane.
  When setting a sequence of rows or [row fn], in the latter case, fn is
  called to perform span-setting, in both cases, rows are set to the table-pane"
  [t rows]
  (let [rowseq (.getRows t)]
    (doseq [r rows]
      (if (vector? r)
        (let [[r f] r]
          (.add rowseq r)
          (f t))
        (.add rowseq r)))))

(defproperties TablePane [t]
  :rows
  (do (.remove (.getRows t) 0 (.getLength (.getRows t))) ;; remove all
      (table-pane-add-rows t it))
  (seq (.getRows t))
  "the tables rows, a sequence, create one with `table-pane-row'."

  :cols
  (let [colseq (.getColumns t)]
    (doseq [c it]
      (.add colseq
            (if (or (number? c) (vector? c))
              (table-pane-column :width c)
              c))))
  (seq (.getColumns t))
  "the table rows, a sequence, create one with table-pane-rows or supply a vector of
  widths")

(defcomponent table-pane [args rows]
  ;; rows given here are _added_
  ;; rows given with set-properties are set
  (with-component [t TablePane]
    (table-pane-add-rows t rows)))

(defproperties TablePane$Row [r]
  :height (set-relative-size .setHeight r it) (get-relative-size .getHeight r) "height, [10] means a relative height."
  :highlighted (.setHighlighted r (boolean it)) (.isHighlighted r) "highlighted flag"
  :visible (.setVisible r (boolean it)) (.isVisible r) "row visibility flag")

(defcomponent table-pane-row [args components]
  ;; components: a seq of components or vectors
  ;; where vectors: [component [col-span row-span]], *,nil means, leave as-is.
  (let [r (with-component [r TablePane$Row]
            (doseq [c components]
              (.add r (if (vector? c) (first c) c))))]
    [r (fn [table-pane]
         ;; set column and/or row span
         (doseq [c components]
           (when (vector? c)
             (let [[c [col-idx row-idx]] c]
               (when (number? col-idx) (TablePane/setColumnSpan c col-idx))
               (when (number? row-idx) (TablePane/setRowSpan c row-idx))))))]))

(set-documentation table-pane (TablePane.) :keys & rows)
(set-documentation table-pane-column (TablePane$Column.) :keys)
(set-documentation table-pane-row (TablePane$Row.) :keys & components)

;;  menus

(defproperties Menu [m]
  :active-item nil (.getActiveItem m) "The active item, read-only."
  :item nil (.getItem m) "The parent item of this menu, read-only."
  :sections nil (.getSections m) "The sequence of `menu-section's, read-only.")

(defcomponent menu [args menu-sections]
  (with-component [m Menu]
    (doseq [c menu-sections]
      (.add (.getSections m) c))))

(defproperties Menu$Section [m]
  :name (.setName m (str it)) (.getName m) "The sections name."
  :items
  (do (.remove m 0 (.getLength m)) ;; remove all
      (doseq [items it]
        (.add m it)))
  (seq (.iterator m))
  "The sections `menu-item's."
  
  :menu nil (.getMenu m) "The menu associated with the section.")

(defcomponent menu-section [args menu-items]
  (with-component [m Menu$Section]
    (doseq [i menu-items]
      (.add m i))))

(defproperties Menu$Item [m] ;; a menu is a subclass of Button
  :menu (.setMenu m it) (.getMenu m) "The items Menu"
  :name (.setName m (str it)) (.getName m) "a name"
  :active (.setActive m (boolean it)) (.isActive m) "active state, a boolean."
  :enabled (.setEnabled m (boolean it)) (.isEnabled m) "Enabled flag, boolean.")

(defcomponent menu-item [args]
  (with-component [m Menu$Item]))

(set-documentation menu (Menu.) :keys & menu-sections)
(set-documentation menu-section (Menu$Section.) :keys & menu-items)
(set-documentation menu-item (Menu$Item.) :keys)

;; menu bar

(defproperties MenuBar [m]
  :items
  (let [is (.getItems m)]
    (.remove is 0 (.getLength is))
    (doseq [i it] (.add is it)))
  (seq (.iterator (.getItems m)))
  "A pivot sequence of `menu-bar-item's.")

(defcomponent menu-bar [args menu-bar-items]
  ;; (menu-bar item1 item2 item3) -> adds item 1 2 3
  ;; (menu-bar :items [item1 item2 item3]) -> removes all items and sets items to 1-3
  (with-component [m MenuBar]
    (doseq [i menu-bar-items]
      (.add (.getItems m) i))))

(defproperties MenuBar$Item [m] ;; a subclass of Button
  :menu (.setMenu m it) (.getMenu m) "The items menu."
  :menu-bar nil (.getMenuBar m) "The items menu-bar, read-only."
  :active (.setActive m (boolean it)) (.isActive m) "Active flag, boolean."
  :enabled (.setEnabled m (boolean it)) (.isEnabled m) "Enabled flag, boolean.")

(defcomponent menu-bar-item [args]
  (with-component [m MenuBar$Item]))

(set-documentation menu-bar (MenuBar.) :keys & menu-bar-items)
(set-documentation menu-bar-item (MenuBar$Item.) :keys)

;; menu popup

(defproperties MenuPopup [m]
  :menu (.setMenu m it) (.getMenu m) "The Popups menu.")

(defcomponent menu-popup [args]
  (with-component [m MenuPopup]))

(set-documentation menu-popup (MenuPopup.) :keys)

;; component misc

(defn focused-component []
  (Component/getFocusedComponent))

;; component tree, using :user to identify components

(defn branch-property
  "Return the property which can contain more components."
  [c]
  (condp #(isa? %2 %1) (type c)
    Container :components
    nil))

(defn get-children
  "Given a component, return its children, if any."
  [c]
  (get-property c (branch-property c)))

(defn component-map
  "Return a map of name:(component*), where name is
  (-> % (get-property :user) :name)."
  [root-component]
  (->> (tree-seq branch-property get-children root-component)
       (map #(vector (get-property % :user-name) %))
       (reduce (fn [m [name comp]]
                 (update-in m [name] conj comp))
               {})))

(defn find-components
  "Beginning at root, return all components matching expr.
  expr may be a keyword, symbol, regex, set or predicate function.
  Symbols and keywords are compared with the :user-name.
  Regexes are matched against the user-name.
  Functions are called for each component.
  Sets are intersected with :user-tags, and match on a nonempty result."
  [root-component expr]  
  (filter (cond (nil? expr) (constantly false)
                (fn? expr) expr
                (instance? java.util.regex.Pattern expr)
                  #(re-matches expr (-> % (get-property :user-name) name))
                (set? expr)
                  (if (next expr)
                    #(-> % (get-property :user-tags) (set/intersection expr) empty? not)
                    #(contains? (get-property % :user-tags) (first expr)))
                :else 
                  #(= expr (get-property % :user-name)))
          (tree-seq branch-property get-children root-component)))

(defn find-component
  "Same as find-components but return the first component only.
  Throw an exception if more than one component has been found."
  [root-component name-f-or-regex]
  (let [[h & t] (find-components root-component name-f-or-regex)]
    (if (seq t)
      (throwf "More than one component found for %s and clause %s"
              root-component name-f-or-regex)
      h)))

;; slow!
(defn component-tuple [c]
  (let [p (get-properties c)]
    {:component c
     :type (.getSimpleName (class c))
     :user-name (:user-name p)
     :properties p}))

(defn component-relation
  "return a relation of components"
  [root]
  (set (map component-tuple (tree-seq branch-property get-children root))))