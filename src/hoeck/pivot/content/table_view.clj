;   Copyright (c) 2010, Erik Soehnel All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns hoeck.pivot.content.table-view
  (:use clojure.contrib.except
        clojure.contrib.pprint
        hoeck.pivot.content ;; editor
        hoeck.pivot.components
        hoeck.pivot.listeners
        hoeck.pivot.datastructures)
  (:import (org.apache.pivot.wtk Keyboard Keyboard$KeyCode
                                 TableView, TableView$Column
                                 TableView$CellRenderer TableView$RowEditor
                                 TableView$RowEditor$RowEditorListenerList)
           (org.apache.pivot.util Vote)
           (java.text DateFormat DecimalFormat)))

;;; table-view-editor

(defn reposition [tv-editor]
  (let [{:keys [tv row-index col-index edit-component popup]} @(.state tv-editor)
        ;; Get the cell bounds
        cb (let [cb (.getCellBounds tv row-index col-index)]
             (.scrollAreaToVisible tv cb)
             (.getVisibleArea cb))
        coords (.mapPointToAncestor tv (.getDisplay tv)
                                   (.x cb)
                                   (.y cb))]
    ;; Position the popup/editor to fit over the cell bounds
    (set-property edit-component :preferred-width (.width cb))
    (set-property popup :location
                  [(.x coords)
                   (+ (.y coords)
                      (/ (- (.height cb)
                            (get-property edit-component :preferred-height))
                         2))])))

(defn basic-editor-setup
  "installs listeners in the popup-window, the popups parent window
  and in the table-view."
  [e edit-component]
  (let [tv (:table-view @(:state e))
        edit-component (-> e :state deref :editor component)
        popup (window edit-component)
        mouse-l (container-mouse-listener
                 {c :container [x y] :int}
                 :mouse-down (do (when-not (= (.getComponentAt x y)
                                              popup)
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
                 {w :window, d :display, o :owner}
                 :preview-window-open Vote/APPROVE
                 :preview-window-close Vote/APPROVE
                 :window-opened
                 (do (add-listener (get-property w :display) mouse-l)
                     (add-listener tv comp-l tv-l tv-row-l))
                 :window-closed
                 (do (remove-listener d mouse-l)
                     (remove-listener tv comp-l tv-l tv-row-l)
                     (.moveToFront o)
                     (reset! (:state e) nil)))]
    (add-listener popup popup-l)
    (add-listener edit-component
                  (component-key-listener
                   {keycode :int}
                   :key-pressed (if (= keycode Keyboard$KeyCode/ENTER)
                                  (.saveChanges e)
                                  (.cancelEdit e))))
    popup-l))

(defn get-tuple [tv row-idx]
  (.get (get-property tv :data) row-idx))

(defn get-column-key [tv col-idx]
  (let [c (.get (get-property tv :cols) col-idx)]
    (keyword (get-property c :name))))

(deftype TableViewEditor [editor-ctor state] :as this
  TableView$RowEditor
  (editRow [tv, row-index, col-index]
    (let [ll (TableView$RowEditor$RowEditorListenerList.)
          vote (.previewEditRow ll row-index col-index)]
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
          (reset! state (atom {:tv tv
                               :row-idx row-index
                               :col-idx col-index
                               :editor editor}))
          (let [popup (basic-editor-setup this)]
            (swap! state assoc :popup popup)
            (.open popup (get-property tv :window))
            (on-open editor)
            (.rowEditing ll this tv row-index col-index)))
        (.editRowVetoed ll this vote))))
  (getRowEditorListeners [] (:row-editor-listeners @state))
  (cancelEdit
   []
   (-> @state :popup .close)
   (-> @state
       :row-editor-listeners
       (.editCancelled this (:tv @state) (:row-idx @state) (:col-idx @state)))
   (reset! state {}))
  (isEditing [] (boolean (:editing @state)))
  (saveChanges
   []
   (let [{:keys [tv row-idx col-idx
                 row-editor-listeners
                 editor]} @state
         changes (make-dictionary
                  {(get-column-key tv col-idx)
                   (value editor)})
         vote (.previewSaveChanges this tv row-idx col-idx changes)]
     (if (= vote Vote/APPROVE)
       (let [tup (merge (get-tuple tv row-idx) changes)
             data (get-property tv :data)]
         ;; Notifying the parent will close the popup
         (if-not (.getComparator data)
           (.put data row-idx tup)
           (do (.remove data row-idx 1)
               (.add data tup)
               (let [i (.indexOf data tup)]
                 (set-property tv :selected-index i)
                 (.scrollAreaToVisible tv (.getRowBounds tv i)))))
         (.changesSaved row-editor-listeners this tv row-idx col-idx))
       (.saveChangesVetoed row-editor-listeners this vote)))))

(defn table-view-editor [editor-ctor]
  (TableViewEditor editor-ctor (atom {})))

(defproperties TableView [tv]
  :editor (.setRowEditor tv (table-view-editor it)) (.getRowEditor tv)
  "The row editor, see hoeck.pivot.content for implementations.")


;; renderer

  ;; "Returns a TableView$CellRenderer implemented through the render-f function.
  ;; Calling render-f without an argument should return a component (i.e. a Label)
  ;; which is used to render data cells.
  ;; Upon rendering, render-f is called with a map of arguments:
  ;;   :component :value :table-view :column 
  ;;   :row-selected? :row-highlighted? :row-disabled?
  ;; and should set the given :component according to its needs, eg:
  ;;   (when row-highlighted? (label :self (:component args) :font [\"Arial\" :bold 12]))."
(deftype TableViewCellRenderer [r]
  TableView$CellRenderer
  ;; TableView$CellRenderer
  ;;void render(Object value, TableView tableView, TableView.Column column, boolean rowSelected, boolean rowHighlighted, boolean rowDisabled)
  ;;void render(Object value, int rowIndex, int columnIndex, TableView tableView, String columnName, boolean rowSelected, boolean rowHighlighted, boolean rowDisabled) 
  (render [value,
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
  (getStyles [] (.getStyles (component r)))
  ;; ConstrainedVisual    
  ;;int getPreferredHeight(int width) Returns the visual's preferred height given the provided width constraint.
  (getPreferredHeight [width] (.getPreferredHeight (component r) width))
  ;;Dimensions getPreferredSize() Returns the visual's unconstrained preferred size.
  (getPreferredSize [] (.getPreferredSize (component r)))
  ;;int getPreferredWidth(int height) Returns the visual's preferred width given the provided height constraint.
  (getPreferredWidth [height] (.getPreferredWidth (component r) height))
  ;;void setSize(int width, int height) Sets the visual's render size.
  (setSize [width height] (doto (component r)
                            (.setSize width height)
                            (.validate)))
  (getBaseline [width height] (.getBaseLine (component r) width height))
  ;; Visual
  ;;int getHeight() Returns the visual's height.
  (getHeight [] (.getHeight (component r)))
  ;;int getWidth() Returns the visual's width.
  (getWidth [] (.getWidth (component r)))
  ;;void paint(Graphics2D graphics) Paints the visual.
  (paint [graphics] (.paint (component r) graphics))
  (getBaseline [] (.getBaseLine (component r))))

(defn set-cell-renderer-styles
  "given the argument map of table-view-cell-renderer, set the columns
   font and color styles according to the styles defined in the table-view."
  [c args]
  (let [tv (or (:table-view args) (throwf ":table-view missing in cell-renderer"))
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

(defn table-view-cell-renderer [renderer]
  (TableViewCellRenderer (renderer)))

(defproperties TableView$Column [t]
  :renderer (.setCellRenderer t (table-view-cell-renderer it)) (.getCellRenderer t)
  "The row editor, see hoeck.pivot.content for implementations.")

;; editor-dispatch, each col his own editor

(defn column-dispatch-editor
  "an editor which looks in the current columns userdata for a suitable
  ::editor."
  []
  (fn [{n :column-name
        tv :table-view
        :as args}]
    (when-let [ed (-> tv
                      (get-property :columns)
                      (get n)
                      (get-property :editor))]
      (ed args))))

(defproperties TableView$Column [tc]
  :editor
  (.setUserdata tc (assoc (or (.getUserData tc) {})
                     ::editor it))
  (get (.getUserData tc) ::editor)
  "Sets the editor to user for this column, requires a proper table-view :editor to be set.")


;;; editor & renderer implementations

(deftypec text-input-editor [ti]
  ([{v :value}] [(text-input :text v)]) ;; ctor
  HasComponent
  (component [] ti)
  Editor
  (value [] (get-property ti :text))
  (on-open [] (doto ti
                  (.selectAll)
                  (.requestFocus))))

(deftypec text-renderer [l]
  ([] [(label)])
  HasComponent
  (component [] (doto l (.validate)))
  Renderer
  (render [argm]
          (set-property l :text (str (:value argm)))
          (set-cell-renderer-styles l argm)))
          
