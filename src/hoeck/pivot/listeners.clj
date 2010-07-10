;   Copyright (c) 2010, Erik Soehnel All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns hoeck.pivot.listeners
  (:require hoeck.pivot.components)
  (:import (org.apache.pivot.wtk ;; Listeners
	    AccordionAttributeListener AccordionListener
	    AccordionSelectionListener ActionClassListener ActionListener
	    ActivityIndicatorListener AlertListener BorderListener
	    BoxPaneListener ButtonGroupListener
	    ButtonListener
	    ButtonPressListener ButtonStateListener CalendarButtonListener
	    CalendarButtonSelectionListener CalendarListener
	    CalendarSelectionListener CardPaneListener ClipboardContentListener
	    ComponentClassListener ComponentDataListener
	    ComponentDecoratorListener ComponentKeyListener ComponentListener
	    ComponentMouseButtonListener ComponentMouseListener
	    ComponentMouseWheelListener ComponentStateListener ContainerListener
	    ContainerMouseListener DialogCloseListener DialogStateListener
	    ExpanderListener FileBrowserListener FileBrowserSheetListener
	    FormAttributeListener FormListener FrameListener ImageViewListener
	    LabelListener ListButtonListener ListButtonSelectionListener
	    ListViewItemListener ListViewItemStateListener ListViewListener
	    ListViewSelectionListener Menu$ItemListener Menu$SectionListener
	    MenuBar$ItemListener MenuBarListener MenuButtonListener
	    MenuItemSelectionListener MenuListener MenuPopupListener
	    MenuPopupStateListener MeterListener MovieViewListener PromptListener
	    RollupListener RollupStateListener ScrollBarListener
	    ScrollBarValueListener ScrollPaneListener SeparatorListener
	    SheetCloseListener SheetStateListener SliderListener
	    SliderValueListener SpinnerItemListener SpinnerListener
	    SpinnerSelectionListener SplitPaneListener TabPaneAttributeListener
	    TabPaneListener TabPaneSelectionListener TablePaneAttributeListener
	    TablePaneListener TableViewColumnListener TableViewHeaderListener
	    TableViewHeaderPressListener TableViewListener TableViewRowListener
            TableView$RowEditorListener
	    TableViewSelectionListener TextAreaCharacterListener TextAreaListener
	    TextAreaSelectionListener TextInputCharacterListener
	    TextInputListener TextInputSelectionListener TextInputTextListener
	    TreeViewBranchListener TreeViewListener
	    TreeViewNodeListener TreeViewNodeStateListener
	    TreeViewSelectionListener ViewportListener
	    WindowActionMappingListener WindowClassListener WindowListener
	    WindowStateListener
            ;; non listeners:
            Action
            ;; component, for ComponentClassListener
            Component)
           (org.apache.pivot.collections FilteredListListener
                                         ListListener
                                         MapListener
                                         QueueListener
                                         SetListener
                                         StackListener)
           (org.apache.pivot.util ListenerList)
	   (java.lang.reflect ParameterizedType)))

(defn listener-throwf [fmt & args]
  (throw (Exception. (apply format fmt args))))

;; delay-wrapped, required only at compiletime for macroexpansion
(def listeners (delay [ ;; org.apache.pivot.wtk.*		
                       AccordionAttributeListener AccordionListener
                       AccordionSelectionListener ActionClassListener ActionListener
                       ActivityIndicatorListener AlertListener BorderListener
                       BoxPaneListener ButtonGroupListener
                       ButtonListener
                       ButtonPressListener ButtonStateListener CalendarButtonListener
                       CalendarButtonSelectionListener CalendarListener
                       CalendarSelectionListener CardPaneListener ClipboardContentListener
                       ComponentClassListener ComponentDataListener
                       ComponentDecoratorListener ComponentKeyListener ComponentListener
                       ComponentMouseButtonListener ComponentMouseListener
                       ComponentMouseWheelListener ComponentStateListener ContainerListener
                       ContainerMouseListener DialogCloseListener DialogStateListener
                       ExpanderListener FileBrowserListener FileBrowserSheetListener
                       FormAttributeListener FormListener FrameListener ImageViewListener
                       LabelListener ListButtonListener ListButtonSelectionListener
                       ListViewItemListener ListViewItemStateListener ListViewListener
                       ListViewSelectionListener Menu$ItemListener Menu$SectionListener
                       MenuBar$ItemListener MenuBarListener MenuButtonListener
                       MenuItemSelectionListener MenuListener MenuPopupListener
                       MenuPopupStateListener MeterListener MovieViewListener PromptListener
                       RollupListener RollupStateListener ScrollBarListener
                       ScrollBarValueListener ScrollPaneListener SeparatorListener
                       SheetCloseListener SheetStateListener SliderListener
                       SliderValueListener SpinnerItemListener SpinnerListener
                       SpinnerSelectionListener SplitPaneListener TabPaneAttributeListener
                       TabPaneListener TabPaneSelectionListener TablePaneAttributeListener
                       TablePaneListener TableViewColumnListener TableViewHeaderListener
                       TableViewHeaderPressListener TableViewListener TableViewRowListener
                       TableViewSelectionListener TextAreaCharacterListener TextAreaListener
                       TextAreaSelectionListener TextInputCharacterListener
                       TextInputListener TextInputSelectionListener TextInputTextListener
                       TreeViewBranchListener TreeViewListener
                       TreeViewNodeListener TreeViewNodeStateListener
                       TreeViewSelectionListener ViewportListener
                       WindowActionMappingListener WindowClassListener WindowListener
                       WindowStateListener
                       ;; org.apache.pivot.collections.*
                       FilteredListListener
                       ListListener
                       MapListener
                       QueueListener
                       SetListener
                       StackListener]))

;; listeners: construction

(defn get-methods [c]
  (let [c (if (isa? (type c) Class) c (type c))]
    (into {} (map vector (map #(.getName %) (.getMethods c)) (.getMethods c)))))

;; map of listener-names -> map of method-name ->
;; java.lang.reflect.Method
;; wrapped in a delay, only required at macroexpansion time
(def listener-map (delay (zipmap (map #(.getSimpleName %) (force listeners))
                                 (map get-methods (force listeners)))))

;; map of listener names (without the package prefix) to
;; full listener names
(def listener-classname-map
     (delay (zipmap (map #(.getSimpleName %) (force listeners))
                    (map #(symbol (.getName %)) (force listeners)))))

(defn lisp-to-camelcase [first-letter-uppercase? symbol-or-keyword]
  (when symbol-or-keyword
    (let [sn (.split (name symbol-or-keyword) "-")
          upcase-first #(str (.toUpperCase (.substring % 0 1)) (.substring % 1))]
      (apply str (cons (if first-letter-uppercase? 
                         (upcase-first (first sn))
                         (first sn))
                       (map upcase-first (next sn)))))))

(defn listener-name
  "Return a listener classname from a keyword or symbol (look for the java-fied
  name in listener-map).
  Dashes are transformed to camelcase.
  Keywords must name the listener without the trailing -listener.
  Example: window-state-listener and :window-state.
  Throw an exception if no listener with the given name can be found.
  Uses `listener-map' for lookup."
  [k]
  ;; lookup using listener classname conventions: FooBarListener
  (let [cc (lisp-to-camelcase true k)
        classname (if (keyword? k) (str cc "Listener") cc)]
    (if ((force listener-map) classname)
      classname
      (listener-throwf "unknown listener: %s (%s)" k classname))))

(defn listener-method-name
  "Return a listener method given the listeners classname and a keyword.
  Use the `listener-map' to look up methods.
  If method-key is nil and listener has only one method, return it. Otherwise
  throw an exception.
  Throw an exception when the method cannot be found."
  [listener-classname method-key] 
  (let [methods (listener-map listener-classname)]
    (if (and (nil? method-key) (= 1 (count methods)))
      (-> methods first key)
      ;; lookup using listener method naming conventions: blaBlaFooChanged()
      (let [name-ch (str (lisp-to-camelcase false method-key) "Changed")
            name (lisp-to-camelcase false method-key)]
        (cond (methods name-ch) name-ch
              (methods name) name
              :else (listener-throwf "unknown listener method: %s (%s, %s)" method-key name-ch name))))))

(defn lispify-camelcase [j]
  (let [m (re-matcher #"[A-Z][a-z]*" (str (.toUpperCase (.substring j 0 1)) (.substring j 1)))]
    (.toLowerCase (apply str (interpose "-" (take-while identity (repeatedly #(re-find m))))))))

(defn class->keyword
  "Generate keywords from method-parameters."
  [c]
  (let [n (.getName c)
        x (first (re-seq #".*\.(.*)" n))]
    (if x 
      (-> x second lispify-camelcase keyword)
      (-> n lispify-camelcase keyword))))

(defn method-argument-keys [method]
  (map class->keyword (.getParameterTypes method)))

(defn get-listener-method-argmap
  "return an argvector and a map which binds those args to keywords. Keywords have their 
  names from the corresponding argument types.
  Two or more arguments of the same type are packed into a vector, as they often denote
  things like [width height], [currentIdx, prevIdx] or [x y].
  The argmap always contains :this, referencing the first arg in argvec and
  :method with the method's lispified keyworded methodname."
  [method]
  (let [argmap-assoc (fn [m [k v]]
                       (assoc m k (if-let [x (get m k)]
                                    (if (vector? x) ;; box only if there a are two or more same keys in the methods argmap
                                      (conj x v)
                                      (conj [x] v))
                                    v)))
        keys (method-argument-keys method)
        args (vec (take (count keys) (repeatedly gensym)))]
    [(assoc (reduce argmap-assoc {} (map vector keys args))
       :method
       (-> method .getName lispify-camelcase keyword))
     args]))

(comment (get-listener-method-argmap (val (first (val (first listener-map)))))
         (get-listener-method-argmap (((force listener-map) "ComponentListener") "preferredSizeChanged"))
         (get-listener-method-argmap (((force listener-map) "WindowStateListener") "windowClosed")))

(defmacro def-listener-type
  "Generate an implementation of the given listener interface using deftype.
  The name will be the lispified classname plus \"*\".
  The type will have one field, a function called with hashmap of arguments:
  :method, :this and lispified keywords of the method-argument types.
  If one method has two or more arguments of the same type, they a packed into
  a clojure vector, eg.: {:method :mouse-down :int [x y]}).
  The type will implement clojure.lang.IPersistentMap."
  [listener-cname]
  (let [method-m ((force listener-map) listener-cname)
        m (keys method-m) ;; the methods to implement
        f (gensym)
        this (gensym)
        dt-cname (symbol (str listener-cname "Impl"))]
    `(do
       ;; deftype implementing the listener iface
       ;; eg: ContainerMouseListenerImpl
       (deftype ~dt-cname [~f]
           clojure.lang.IPersistentMap
           ~((force listener-classname-map) listener-cname)
           ~@(map (fn [method-name]
                    (let [[argmap argvec] (get-listener-method-argmap (method-m method-name))]
                      `(~(symbol method-name) ~(vec (concat [this] argvec)) (~f ~(assoc argmap :this this)))))
                  m))
       ;; ctor-fn
       ;; eg: container-mouse-listener*
       (defn ~(symbol (str (lispify-camelcase listener-cname) "*")) [~f]
         (new ~dt-cname ~f)))))

(defmacro define-listener-types
  "Define types for all pivot listeners."
  []
  `(do ~@(map (fn [n] `(def-listener-type ~n)) (keys (force listener-map)))))

(define-listener-types)

;; listeners: adding & removing

(defn get-implemented-listener-type
  "return the implemented listener of listener-impl l."
  ;; should return the most specific Listener
  [l]
  (let [s (supers (type l))]
    (first (filter #(contains? listener-map (.getSimpleName %)) s))))

(defn listener-list-getter?
  "decide wether a given java.lang.reflect.Method is a ListenerList getter method,
  eg: compnent.getComponentMouseListers()"
  [method]
  (and (-> method .getReturnType (isa? ListenerList))
       (re-matches #"get.*Listeners" (-> method .getName))))

(defn get-listener-list-getter-type-from-name
  "Given a ListenerList getter, return the element type of that ListenerList
  by guessing the type from the name of the getter."
  [method]
  (let [n (.getName method)
        lname (str "org.apache.pivot.wtk." (.substring n 3 (dec (count n))))]
    (try (Class/forName lname)
         (catch ClassNotFoundException e
           (listener-throwf "listener getter type %s not found in %s" lname n)))))


;; Listener-type to ListenerList getter method map generation
;; invoke manually when neccessary to rebuild listener-type-method-map
;;(defn- generate-listener-method-map []
;;  (let [lgetters (distinct (filter listener-list-getter?
;;                                   (mapcat #(.getMethods %)
;;                                           hoeck.pivot.components/components)))]
;;    (zipmap (map #(-> % get-listener-list-getter-type .getName symbol) lgetters)
;;            (map #(-> % .getName) lgetters))))

;; use reflection to add or remove listeners

;;; from rhickey: http://paste.lisp.org/display/67182
(defn jcall [obj name & args]
  (clojure.lang.Reflector/invokeInstanceMethod obj (str name)
    (if args (to-array args) clojure.lang.RT/EMPTY_ARRAY)))

;;; from rhickey: http://paste.lisp.org/display/67182
(defn jfn [name] #(apply jcall %1 name %&))

;; map: listener-type -> listener-list getter method
(def listener-type-method-map
     {RollupListener "getRollupListeners",
      FrameListener "getFrameListeners",
      BorderListener "getBorderListeners",
      SpinnerListener "getSpinnerListeners",
      RollupStateListener "getRollupStateListeners",
      ScrollPaneListener "getScrollPaneListeners",
      MenuItemSelectionListener "getMenuItemSelectionListeners",
      TextAreaCharacterListener "getTextAreaCharacterListeners",
      TextAreaListener "getTextAreaListeners",
      ActivityIndicatorListener "getActivityIndicatorListeners",
      CalendarSelectionListener "getCalendarSelectionListeners",
      ListViewItemStateListener "getListViewItemStateListeners",
      AccordionListener "getAccordionListeners",
      ButtonListener "getButtonListeners",
      FileBrowserSheetListener "getFileBrowserSheetListeners",
      TableViewColumnListener "getTableViewColumnListeners",
      ViewportListener "getViewportListeners",
      SeparatorListener "getSeparatorListeners",
      TabPaneAttributeListener "getTabPaneAttributeListeners",
      AccordionSelectionListener "getAccordionSelectionListeners",
      ScrollBarValueListener "getScrollBarValueListeners",
      ContainerMouseListener "getContainerMouseListeners",
      MenuPopupListener "getMenuPopupListeners",
      ComponentKeyListener "getComponentKeyListeners",
      TreeViewNodeStateListener "getTreeViewNodeStateListeners",
      TableViewHeaderPressListener "getTableViewHeaderPressListeners",
      FileBrowserListener "getFileBrowserListeners",
      TableViewSelectionListener "getTableViewSelectionListeners",
      MovieViewListener "getMovieViewListeners",
      AccordionAttributeListener "getAccordionAttributeListeners",
      LabelListener "getLabelListeners",
      ComponentDataListener "getComponentDataListeners",
      WindowActionMappingListener "getWindowActionMappingListeners",
      DialogStateListener "getDialogStateListeners",
      TableViewListener "getTableViewListeners",
      AlertListener "getAlertListeners",
      CalendarButtonSelectionListener "getCalendarButtonSelectionListeners",
      ComponentMouseWheelListener "getComponentMouseWheelListeners",
      TreeViewNodeListener "getTreeViewNodeListeners",
      SliderValueListener "getSliderValueListeners",
      MenuPopupStateListener "getMenuPopupStateListeners",
      CardPaneListener "getCardPaneListeners",
      Menu$ItemListener "getItemListeners",
      TablePaneListener "getTablePaneListeners",
      ListViewItemListener "getListViewItemListeners",
      TableViewHeaderListener "getTableViewHeaderListeners",
      TabPaneSelectionListener "getTabPaneSelectionListeners",
      TextInputSelectionListener "getTextInputSelectionListeners",
      ContainerListener "getContainerListeners",
      WindowStateListener "getWindowStateListeners",
      ComponentStateListener "getComponentStateListeners",
      ComponentListener "getComponentListeners",
      SheetStateListener "getSheetStateListeners",
      MeterListener "getMeterListeners",
      SpinnerSelectionListener "getSpinnerSelectionListeners",
      TablePaneAttributeListener "getTablePaneAttributeListeners",
      ListButtonSelectionListener "getListButtonSelectionListeners",
      ButtonStateListener "getButtonStateListeners",
      TreeViewBranchListener "getTreeViewBranchListeners",
      WindowListener "getWindowListeners",
      ListButtonListener "getListButtonListeners",
      PromptListener "getPromptListeners",
      MenuButtonListener "getMenuButtonListeners",
      ButtonPressListener "getButtonPressListeners",
      CalendarListener "getCalendarListeners",
      ComponentDecoratorListener "getComponentDecoratorListeners",
      FormAttributeListener "getFormAttributeListeners",
      SplitPaneListener "getSplitPaneListeners",
      ListViewListener "getListViewListeners",
      TextInputCharacterListener "getTextInputCharacterListeners",
      SpinnerItemListener "getSpinnerItemListeners",
      TreeViewListener "getTreeViewListeners",
      ComponentMouseButtonListener "getComponentMouseButtonListeners",
      ImageViewListener "getImageViewListeners",
      MenuBarListener "getMenuBarListeners",
      ;;ComponentClassListener "getComponentClassListeners", ;; static!
      SliderListener "getSliderListeners",
      TabPaneListener "getTabPaneListeners",
      TextInputListener "getTextInputListeners",
      TableViewRowListener "getTableViewRowListeners",
      TextAreaSelectionListener "getTextAreaSelectionListeners",
      ExpanderListener "getExpanderListeners",
      WindowClassListener "getWindowClassListeners",
      ComponentMouseListener "getComponentMouseListeners",
      ScrollBarListener "getScrollBarListeners",
      ListViewSelectionListener "getListViewSelectionListeners",
      BoxPaneListener "getBoxPaneListeners",
      TreeViewSelectionListener "getTreeViewSelectionListeners",
      MenuListener "getMenuListeners",
      CalendarButtonListener "getCalendarButtonListeners",
      TextInputTextListener "getTextInputTextListeners",
      FormListener "getFormListeners",
      TableView$RowEditorListener "getRowEditorListeners"
      ;; CollectionListeners
      ListListener "getListListeners"
      MapListener "getMapListeners"
      })

(defn get-listener-list-getter-names
  "Given a listener, return all possible getters for its listenerlist."
  [listener]
  (let [listener-ifaces (->> (.getInterfaces (class listener))
                             (filter #(contains? listener-type-method-map %)))
        l-getter-names (map listener-type-method-map listener-ifaces)]
    (if (empty? l-getter-names)
        (listener-throwf "cannot find any listener-list-getters for %s" (type listener))
        l-getter-names)))

(defn get-listener-list
  "Given a listener and an object, return a listenerlist for a listener-type
  listener implements.
  Throw an Exception if no listenerlist matches."
  [object listener]
  (let [listener-list (->> (get-listener-list-getter-names listener)
                           (map #(try (jcall object %) (catch Exception e nil)))
                           (remove nil?)
                           first)]
    (if (nil? listener-list)
      (listener-throwf "Don't know how to get ListenerList for object %s of type %s"
              object (type object))
      listener-list)))

(defn add-listener
  "Add listener(s) to objects, return nil."
  ([object listener]
     (.add (get-listener-list object listener) listener))
  ([object listener & more-listeners]
     (add-listener object listener)
     (apply add-listener object more-listeners)))

(defn remove-listener
  "Remove listener(s) from object."
  ([object listener]
     (.remove (get-listener-list object listener) listener))
  ([object listener & more-listeners]
     (remove-listener object listener)
     (apply remove-listener object more-listeners)))

;; component class listener

(defn clear-component-class-listeners []
  (let [ls (Component/getComponentClassListeners)]
    (doseq [l (seq ls)] (.remove ls l))))

(defn add-component-class-listener
  "Registers a ComponentClassListener which is fired when the focus changes and
  calls f with the previously focused component. 
  Use h.p.components/focused-component to determine the currently focused c."
  [f]
  (let [ls (Component/getComponentClassListeners)]
    (.add ls (proxy [ComponentClassListener] []
               ;;void focusedComponentChanged(Component previousFocusedComponent)
               (focusedComponentChanged [c] (f c))))))

;; listener macros: (autogenerated)
;; for each pivot listener class, create a macro which creates such a listener
;; e.g: for the ComponentMouseListener, a component-mouse-listener macro is
;;      generated, which in turn expands to:
;;        (listener :component-mouse * (fn [args] ..))

(defmacro listener-macro
  "Define a listener using a macro.
   Listener name should be a lispified name of the listener.
   binding-expr must be a left-side let form that is bound to the
   listener-methods argument-map. If its a vector, use it to destructure
   the argmap with the given keys: {:keys -the-vector-}.
   Body must be structured like a condp body:
   (condp = listener-metod-name ~@body)"
  [listener-name binding-expr & body]
  ;; instantiate the listener type
  `(~(symbol "hoeck.pivot.listeners" (str listener-name "*"))
             (fn [argmap#]
               (let [~(cond (symbol? binding-expr) binding-expr
                            (vector? binding-expr) {:keys binding-expr}
                            (map? binding-expr) binding-expr)
                     argmap#]
                 (condp = (:method argmap#)
                   ~@body)))))

(defmacro def-listener-macro
  "define a listener-macro for the given listener-name"
  [listener-name]
  `(defmacro ~listener-name [~'binding-expr & ~'body]
     `(listener-macro ~'~listener-name ~~'binding-expr ~@~'body)))

(defn all-listener-names
  "Given the listener-map, return seq of listener-names (symbols)."
  ([] (all-listener-names (force listener-map)))
  ([listener-map]
     (->> listener-map keys (map lispify-camelcase) (map symbol))))

(defmacro define-listener-macros
  "define listener macros for all known listeners returned by
  `all-listener-names'."
  []
  `(do ~@(map #(list `def-listener-macro %) (all-listener-names))))

(define-listener-macros)


;; actions

(defn action
  "Given a function, return a org.apache.pivot.wtk.Action calling (f)
  on Action.perform()."
  [f]
  (proxy [Action] []
    (perform [] (f))))

;; meta listeners

(defn data-change-listener*
  "Returns an object that implements several <component>-change-listeners
  at once: text-input, list-button, calendar-button and Button.
  It implements those types of listeners that fire when the components
  displayed data changed."
  [f]
  (proxy [TextInputTextListener ;; void textChanged(TextInput textInput) 
          ListButtonSelectionListener ;; void selectedIndexChanged(ListButton listButton, int previousSelectedIndex) 
          CalendarButtonSelectionListener ;; void selectedDateChanged(CalendarButton calendarButton, CalendarDate previousSelectedDate) 
          ButtonStateListener ;; void stateChanged(Button button, Button.State previousState)
          ] []
    (textChanged [& _] (f))
    (selectedIndexChanged [& _] (f))
    (selectedDateChanged [& _] (f))
    (stateChanged [& _] (f))))

(defmacro data-change-listener
  "Macro wrapper around data-change-listener*."
  [& body]
  `(data-change-listener* (fn [] ~@body)))

