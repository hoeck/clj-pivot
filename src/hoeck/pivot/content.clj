
(ns hoeck.pivot.content
  (:use clojure.contrib.except)
  (:import (org.apache.pivot.wtk Keyboard Keyboard$KeyCode
                                 TableView$CellRenderer TableView$RowEditor)
           (org.apache.pivot.util Vote)
           (java.text DateFormat DecimalFormat)))

;; impose a loading order because there is a circular reference between content and components
(declare label-cell-renderer)
(declare table-view-editor)
(use 'hoeck.pivot.components)
(use 'hoeck.pivot.listeners)
(use 'hoeck.pivot.datastructures)
(require '[hoeck.pivot.relations :as pivot-rel])

;; table-view cell renderers and editors
;; require tableData to be a List of tuples (clojure hashmaps)
;; see hoeck.pivot.relations

(defn set-cell-renderer-styles
  "given the argument map of table-view-cell-renderer, set the columns
  font and color styles according to the styles defined in the table-view."
  [args]
  (let [tv (or (:table-view args) (throwf ":table-view missing in cell-renderer"))
        cmp (or (:component args) (throwf ":component missing in cell-renderer"))
        tv-styles (.getStyles tv)
        color (.get tv-styles
                    (if (and (.isEnabled tv) (not (:row-disabled? args)))
                      (if (:row-selected? args)
                        (if (.isFocused tv)
                          "selectionColor"
                          "inactiveSelectionColor")
                        "color")
                      "disabledColor"))]
    (doto (.getStyles cmp)
      (.put "color" color)
      (.put "font" (.get tv-styles "font")))))

(defn cell-value [col tuple default]
  (get tuple (keyword (get-property col :name)) default))

(defn fixed-label-cell-renderer [text]
  (fn ([] (label :styles {:horizontal-alignment :left
                          :vertical-alignment :center
                          :color [0 0 255]
                          :padding [2 2 2 2]}))
    ([{:keys [component, value, column] :as args}]
       (set-cell-renderer-styles args)
       (set-property component :text text)
       (set-property component :styles {:color [0 0 255]
                                        :text-decoration :underline}))))

(defn label-cell-renderer
  "like a plain cell renderer but renders data from a clojure hashmap instead
  of a pivot dictionary."
  []
  (fn ([] (label :styles {:horizontal-alignment :left
                          :vertical-alignment :center
                          :padding [2 2 2 2]}))
    ([{:keys [component, value, column] :as args}]
       (set-cell-renderer-styles args)
       (set-property component :text (str (cell-value column value ""))))))

(defn label-map-cell-renderer
  "like a label-cell-renderer, but renders values found in display-fn after
  calling (display-fn the-cell-value)."
  [display-fn]
  (fn ([] (label :styles {:horizontal-alignment :left
                          :vertical-alignment :center
                          :padding [2 2 2 2]}))
    ([{:keys [component, value, column] :as args}]
       (set-cell-renderer-styles args)
       (set-property component :text (str (display-fn (cell-value column value nil)))))))

(defn label-timestamp-cell-renderer
  "Render a java.sql.timestamp"
  []
  (fn ([] (label :styles {:horizontal-alignment :left
                          :vertical-alignment :center
                          :padding [2 2 2 2]}))
    ([{:keys [component, value, column] :as args}]
       (set-cell-renderer-styles args)
       (let [cv (cell-value column value nil)]
         (when cv (set-property 
                   component :text 
                   (str (-> (DateFormat/getDateTimeInstance DateFormat/MEDIUM DateFormat/SHORT)
                            (.format cv)))))))))

(defn list-button-renderer
  []
  (fn ([] (list-button))
    ([{:keys [component, value, column] :as args}]
       (set-property component :data (str (cell-value column value ""))))))

;; table-view-editors

;; basically:
;;  create a component
;;  open a popup window with the component as the only content
;;  register listeners:
;;    to close the popup on any non-popup-click or ESC
;;    to update the row on enter (or doubleclick)
;;    etc. pp.

(defn scroll-to-cell
  "Scroll the tableview so that the cell [row, col]-idx is visible.
  Return the bounds of this cell as [x y w h]."
  [tv row-idx col-idx]
  (let [cb (.getCellBounds tv row-idx col-idx)]
    (.scrollAreaToVisible tv cb)
    (get-bounds (.getVisibleArea tv cb))))

(defn table-view-editor-popup
  "Prepare everything to host the edit-component in a popup above the given
  table-cell ad row-idx and col-idx.
  On user cancel commands (ESC, clicked outside) and table-data-changes, 
  close the popup and cancel the editor.
  Returns the popup window."
  [edit-component #^::TableViewEditor editor tv cell-bounds]
  (let [popup (window :auxilliary true
                      :location (let [[x y w h] cell-bounds] 
                                  [x (+ y (/ (- h (.getPreferredHeight edit-component -1))
                                             2))])
                      edit-component)
        ;; listeners (require references to the other listeners)
        cml (container-mouse-listener {cont :container [x y] :int}
              :mouse-down (let [w (.getComponentAt cont x y)]
                            (when-not (identical? popup w)
                              ;; user clicked out of the bounds of this popup
                              (.cancel editor))
                            false)
              :mouse-wheel true
              false)
        ;; cancel editing if any rows are modified while editing
        tvrl (table-view-row-listener _ (.cancel editor))
        ;;  cancel editing on any table-data change
        tvl (table-view-listener {m :method}
              (when (#{:table-data-changed
                       :table-view-row-editor-changed}
                     m)
                (.cancel editor)))
        ;; cancel on ESCAPE, save on ENTER
        ckl (component-key-listener {keycode :int :as args}
              :key-pressed (do (condp = keycode
                                 Keyboard$KeyCode/ESCAPE (.cancel editor)
                                 Keyboard$KeyCode/ENTER (.save editor)
                                 :ignore)
                               false)
              false)
        ;; popup window-listener: edit initialization & finalization
        wsl (window-state-listener {w :window :as args}
              :window-opened (do (add-listener (.getDisplay w) cml)
                                 (add-listener tv tvl tvrl))
              :window-closed (do (remove-listener (:display args) cml)
                                 (remove-listener tv tvl tvrl)
                                 (.requestFocus tv))
              :preview-window-open Vote/APPROVE
              :preview-window-close Vote/APPROVE)]
    (add-listener edit-component ckl)
    (add-listener popup wsl)
    (.open popup (.getWindow tv))
    (.requestFocus edit-component)
    popup))

(defn find-table-view-cell
  "return [name value] of a given table-view-cell at [row,column]"
  [tv row-idx col-idx]
  (let [colname (-> tv .getColumns (.get col-idx) (get-property :name) keyword)]
    [colname (get (.get (.getTableData tv) row-idx) colname)]))
  
(deftype TableViewEditor [state editor] :as this
  TableView$RowEditor
  (edit [tv row-idx col-idx]
    (let [[colname colvalue] (find-table-view-cell tv row-idx col-idx)
          b (scroll-to-cell tv row-idx col-idx)
          _ (reset! state {:tv tv :row-idx row-idx :col-idx col-idx :editor this
                           :colname colname :colvalue colvalue :bounds b})
          e (editor :edit @state) ;; nil means do-not-edit
          p (when e (table-view-editor-popup e this tv b))]
      (if e 
        (swap! state assoc :popup p :component e)
        (.cancel this))))
  (cancel []
    (if-let [p (:popup @state)] (.close p))
    (reset! state nil))
  (save []
    (let [{:keys [colname row-idx tv] :as s} @state]
      (pivot-rel/update (.getTableData tv)
                        row-idx
                        {colname (editor :value s)})
      (.cancel this)))
  (isEditing []
             (not (empty? @state))))

(defn table-view-editor
  "ctor for a table-view editor"
  [f]
  (TableViewEditor (atom {}) f))

(defmacro defeditor
  "Define a function that returns a TableView$RowEditor.
  Docstring is mandatory."
  [editor-name docstring argvec & body]
  (let [st (gensym 'state)]
    `(defn ~editor-name
       ~docstring
       ~argvec
       (fn table-view-editor-fn [method# ~st]
         (condp = method#
           ~@(mapcat (fn [[method-symbol bind-expr & method-body]]
                       `(~(-> method-symbol name keyword)
                         (let ~(conj bind-expr st)
                           ~@method-body)))
                     body))))))

(defeditor exception-editor
  "editor that throws an exception when its invoked."
  [msg]
  (edit [_] (throwf msg))
  (value [_] nil))

(defeditor no-editor
  "editor to not edit a column"
  []
  (edit [_] nil)
  (value [_] nil))

(defeditor text-input-editor
  "Uses a text-input to edit the cell. Stores values as strings."
  []
  (edit [{n :colname v :colvalue [x y w h] :bounds}]
        (doto (text-input :text (str v) :preferred-width w)
          (.selectAll)))
  (value [{ti :component}] (get-property ti :text)))

(defeditor list-button-editor
  "Uses a list-button to choose one from a map of choices. A map value is
  displayed if its key is the current value of the cell.
  If choice-map a function of one argument, the current tuple and it
   should return a map to lookup the displayed-value."
  [choice-map]
  (edit [{n :colname v :colvalue [x y w h] :bounds tv :tv row-idx :row-idx}]
        (let [choice-map (if (fn? choice-map)
                           (choice-map (.get (.getTableData tv) row-idx))
                           choice-map)
              lb (list-button :data (get choice-map v)
                              :preferred-width w
                              :list-data (make-list (vals choice-map)))]
          (add-listener lb (list-button-selection-listener _
                             (pivot-rel/update (.getTableData tv)
                                               row-idx
                                               {n (let [s (.getSelectedItem lb)
                                                           i (first (filter #(-> % val (= s)) choice-map))]
                                                       (key i))})))
          lb))
  (value [_] 'never-called))

(defn editors
  "Editor that invokes a different editor for each column.
  Use 'default to supply a default editor."
  ([columnkey editor & more]
     (editors (apply hash-map columnkey editor more)))
  ([column-editor-map]
     (let [default (get column-editor-map 'default (no-editor))]
       (fn table-view-editor-fn [m {n :colname :as state}]
         ((get column-editor-map n default) m state)))))


;; button renderers






