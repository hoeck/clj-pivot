;   Copyright (c) 2010, Erik Soehnel All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns hoeck.pivot.content.table-view
  (:use hoeck.pivot.content ;; editor
        hoeck.pivot.components
        hoeck.pivot.listeners
        hoeck.pivot.datastructures
        [clojure.set :only [map-invert]])
  (:import (org.apache.pivot.wtk Keyboard Keyboard$KeyCode
                                 TableView, TableView$Column
                                 TableView$CellRenderer TableView$RowEditor
                                 TableView$RowEditor$RowEditorListenerList)
           (org.apache.pivot.util Vote)
           (java.text DateFormat DecimalFormat)))

;;; table-view-editor

(defn reposition
  "Place the given editors popup window centered above the to be edited cell
  of the table view."
  [tv-editor]
  (let [{:keys [table-view row-idx col-idx editor popup]} (get-state tv-editor)
        tv table-view
        edit-component (component editor)
        ;; Get the cell bounds
        cb (let [cb (.getCellBounds tv row-idx col-idx)]
             (.scrollAreaToVisible tv cb)
             (.getVisibleArea tv cb))
        coords (.mapPointToAncestor tv (.getDisplay tv)
                                    (.x cb)
                                    (.y cb))]
    ;; Position the popup/editor to fit over the cell bounds
    (set-property edit-component :preferred-width (.width cb))
    (set-property popup :location
                  [(.x coords)
                   (+ (.y coords)
                      (/ (- (.height cb)
                            (-> edit-component (get-property :preferred-height) (nth 1)))
                         2))])))

(defn basic-editor-setup
  "Creates and returns a popup-window containing the edit-component.
  Installs listeners in the popup-window, the popups parent window
  and in the table-view.
  Places the window over the to be edited table-view cell."
  [e]
  (let [{tv :table-view editor :editor} (get-state e)
        edit-component (component editor)
        popup (window edit-component)
        mouse-l (container-mouse-listener
                 {c :container [x y] :int}
                 :mouse-down (let [clicked-c (.getComponentAt c x y)]
                               (when-not (or (= clicked-c popup)
                                             (and (instance? org.apache.pivot.wtk.ListButton edit-component)
                                                  (= clicked-c (.getListPopup edit-component))))
                                 (.saveChanges e))
                               false)
                 :mouse-wheel true
                 false)
        comp-l (component-listener
                _
                :size-changed (reposition e)
                :location-changed (reposition e))
        tv-l (table-view-listener
              _
              :table-data-changed (.cancelEdit e)
              :row-editor-changed (.cancelEdit e))
        tv-row-l (table-view-row-listener _ (.cancelEdit e))
        popup-l (window-state-listener
                 {w :window, d :display :as args}
                 :preview-window-open Vote/APPROVE
                 :preview-window-close Vote/APPROVE
                 :window-opened
                 (do (add-listener (get-property w :display) mouse-l)
                     (add-listener tv comp-l tv-l tv-row-l)
                     ;; add the listener in charge of calling (reposition)
                     ;; also to the parent component.
                     ;; table-views hosted inside a scrollpane will not
                     ;; receive component size-changed and location-changed
                     ;; events (bug?)
                     (add-listener (get-property tv :parent) comp-l))
                 :window-closed
                 (let [[window owner] w]
                   (do (remove-listener d mouse-l)
                       (remove-listener tv comp-l tv-l tv-row-l)
                       (remove-listener (get-property tv :parent) comp-l)
                       (.moveToFront owner)
                       (set-state e nil))))]
    (add-listener popup popup-l)
    (add-listener edit-component
                  (component-key-listener
                   {keycode :int}
                   :key-pressed (condp = keycode
                                  Keyboard$KeyCode/ENTER (do (.saveChanges e) true)
                                  Keyboard$KeyCode/ESCAPE (do (.cancelEdit e) true)
                                  false)
                   false))
    popup))

(defn get-tuple
  "Return the contents of the table-views data at row row-idx."
  [tv row-idx]
  (.get (get-property tv :data) row-idx))

(defn get-column-key
  "Given the column-index, return the column name of the table-view as a keyword."
  [tv col-idx]
  (let [c (.get (get-property tv :cols) col-idx)]
    (keyword (get-property c :name))))

;; implements the plumbing around defining editors
(deftype CljTableViewEditor [editor-ctor
                             #^{:volatile-mutable true} state
                             #^{:volatile-mutable true} listener-list]
  Stateful
  (get-state [_] state)
  (set-state [_ s] (set! state s))
  TableView$RowEditor
  (editRow [this, tv, row-index, col-index]
           (let [vote (.previewEditRow listener-list this tv row-index col-index)]
             (if (= vote Vote/APPROVE)
               (let [editor (editor-ctor
                             (let [k (get-column-key tv col-index)
                                   tup (get-tuple tv row-index)]
                               {:table-view tv
                                :tuple tup
                                :value (get tup k)
                                :key k
                                :row-idx row-index
                                :col-idx col-index}))]
                 (set! state {:table-view tv
                              :row-idx row-index
                              :col-idx col-index
                              :editor editor})
                 (let [popup (basic-editor-setup this)]
                   (set! state (assoc state :popup popup))
                   (.open popup (get-property tv :window))
                   (reposition this)
                   (on-open editor)
                   (.rowEditing listener-list this tv row-index col-index)))
               (.editRowVetoed listener-list this vote))))
  (getRowEditorListeners [this] listener-list)
  (cancelEdit [this]
              (let [{tv :table-view ri :row-idx ci :col-idx} state]
                (-> state :popup .close) ;; clears state!!!
                (.editCancelled listener-list this tv ri ci)))
  (isEditing [this] (boolean (:editing state)))
  (saveChanges [this]
               (let [{:keys [table-view row-idx col-idx editor]} state
                     tv table-view
                     changes {(get-column-key tv col-idx) (value editor)}
                     changes-dict (make-dictionary changes)
                     vote (.previewSaveChanges listener-list this tv row-idx col-idx changes-dict)]
                 (if (= vote Vote/APPROVE)
                   (let [tup (merge (get-tuple tv row-idx) changes)
                         data (get-property tv :data)]
                     ;; Notifying the parent will close the popup
                     (if-not (.getComparator data)
                       (.update data row-idx tup)
                       (do (.remove data row-idx 1)
                           (.add data tup)
                           (let [i (.indexOf data tup)]
                             (set-property tv :selected-index i)
                             (.scrollAreaToVisible tv (.getRowBounds tv i)))))
                     (.changesSaved listener-list this tv row-idx col-idx))
                   (.saveChangesVetoed listener-list this vote)))))

(defn table-view-editor
  "Constructor-fn to wrap a protocol defined clj-pivot editor in a valid
  pivot TableView$RowEditor corset."
  [editor-ctor]
  (CljTableViewEditor. editor-ctor
                       {}
                       (TableView$RowEditor$RowEditorListenerList.)))

;; provide a property for editors
(defproperties TableView [tv]
  :editor (.setRowEditor tv (table-view-editor it)) (.getRowEditor tv)
  "The row editor, see hoeck.pivot.content for implementations.")


;; renderer

(deftype TableViewCellRenderer [r]
  TableView$CellRenderer
  ;; TableView$CellRenderer
  ;;void render(Object value, TableView tableView, TableView.Column column, boolean rowSelected, boolean rowHighlighted, boolean rowDisabled)
  ;;void render(Object value, int rowIndex, int columnIndex, TableView tableView, String columnName, boolean rowSelected, boolean rowHighlighted, boolean rowDisabled) 
  (render [this,
           value,
           row-idx,
           col-idx,
           table-view,
           column-name,
           row-selected?,
           row-highlighted?,
           row-disabled?]
          (render r {:tuple value
                     :value (get value (keyword column-name))
                     :row-idx row-idx
                     :col-idx col-idx
                     :table-view table-view
                     :column-name column-name
                     :row-selected? row-selected?
                     :row-highlighted? row-highlighted?
                     :row-disabled? row-disabled?})
          nil)
  ;; Renderer   
  ;;Dictionary<String,Object> getStyles() Returns the renderer's style dictionary.
  (getStyles [this] (.getStyles (component r)))
  ;; ConstrainedVisual    
  ;;int getPreferredHeight(int width) Returns the visual's preferred height given the provided width constraint.
  (getPreferredHeight [this width] (.getPreferredHeight (component r) width))
  ;;Dimensions getPreferredSize() Returns the visual's unconstrained preferred size.
  (getPreferredSize [this] (.getPreferredSize (component r)))
  ;;int getPreferredWidth(int height) Returns the visual's preferred width given the provided height constraint.
  (getPreferredWidth [this height] (.getPreferredWidth (component r) height))
  ;;void setSize(int width, int height) Sets the visual's render size.
  (setSize [this width height] (doto (component r)
                            (.setSize width height)
                            (.validate)))
  (getBaseline [this width height] (.getBaseLine (component r) width height))
  ;; Visual
  ;;int getHeight() Returns the visual's height.
  (getHeight [this] (.getHeight (component r)))
  ;;int getWidth() Returns the visual's width.
  (getWidth [this] (.getWidth (component r)))
  ;;void paint(Graphics2D graphics) Paints the visual.
  (paint [this graphics] (.paint (component r) graphics))
  (getBaseline [this] (.getBaseLine (component r))))

(defn set-cell-renderer-styles
  "given the argument map of table-view-cell-renderer, set the columns
   font and color styles according to the styles defined in the table-view."
  [c args]
  (let [tv (or (:table-view args) (throw (Exception. ":table-view missing in cell-renderer")))
        tv-styles (.getStyles tv)
        color (.get tv-styles
                    (if (and (.isEnabled tv) (not (:row-disabled? args)))
                      (if (:row-selected? args)
                        (if (.isFocused tv)
                          "selectionColor"
                          "inactiveSelectionColor")
                        "color")
                      "disabledColor"))]
    (doto (.getStyles c)
      (.put "color" color)
      (.put "font" (.get tv-styles "font")))))

(defn table-view-cell-renderer
  "Given a function which returns an implementation of Renderer, return a
  pivot TableViewCellRenderer implementation."
  [renderer]
  (TableViewCellRenderer. (renderer)))


;;(declare text-renderer)
(defproperties TableView$Column [t]
  :renderer
  (.setCellRenderer t (table-view-cell-renderer it))
  (.getCellRenderer t)
  "The row editor, see hoeck.pivot.content for implementations.")

;; editor-dispatch, each col his own editor

(defn column-dispatch-editor
  "an editor which looks in the current columns userdata for a suitable
  :editor"
  ([& {:as column-editor-map}]
     (fn [{k :key tv :table-view :as args}]
       (when-let [ed (get column-editor-map k)]
         (ed args)))))

;;; editor & renderer implementations

(deftype TextInputEditor [ti]
  HasComponent
  (component [this] ti)
  Editor
  (value [this] (get-property ti :text))
  (on-open [this] (doto ti
                    (.selectAll)
                    (.requestFocus))))

(defn text-input-editor []
  (fn [{v :value}] (TextInputEditor. (text-input :text v))))


(deftype TextRenderer [l]
  HasComponent
  (component [this] (doto l (.validate)))
  Renderer
  (render [this argm]
          (set-property l :text (str (:value argm)))
          (set-cell-renderer-styles l argm)))

(defn text-renderer
  "Simple renderer for rendering cell data as strings using a label."
  []
  #(TextRenderer. (label)))

(deftype CheckboxRenderer [cb value-state-map]
  HasComponent
  (component [this] (doto cb (.validate)))
  Renderer
  (render [this {v :value :as argm}]
          (set-property cb :state (value-state-map v))
          (set-cell-renderer-styles cb argm)))


(defn checkbox-renderer
  "Create a checkbox renderer. Mapping is a function of one argument
  which defines how cell-values should be mapped to
  button-states (:selected/true,
  :unselected/false and :mixed/nil).
  Default mapping is boolean."
  ([] (checkbox-renderer boolean))
  ([mapping-f] #(CheckboxRenderer. (checkbox) mapping-f)))


(deftype ListButtonEditor [lb]
  HasComponent
  (component [_] (doto lb (.validate)))
  Editor
  (value [this] (:text (get-property lb :selected-item)))
  (on-open [this] (doto lb (.requestFocus))))

(defn list-button-editor
  "Simple list-button editor. list-data is a list of hashmaps with :text and :icon keys.
  Upon selection, the :text is stored in the table cell."
  [list-data]
  (fn [{v :value}]
    (ListButtonEditor. (list-button :list-data list-data
                                    :selected-item (->> list-data
                                                        (filter #(= v (:text %)))
                                                        first)))))

