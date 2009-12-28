
;; editor and renderer implementations for various components
(ns hoeck.pivot.editors
  (:use hoeck.pivot.components
        hoeck.pivot.datastructures
        hoeck.pivot.listeners
        clojure.contrib.except)
  (:import (org.apache.pivot.wtk Keyboard Keyboard$KeyCode)
           (org.apache.pivot.util Vote)))


;; table-view-row-editors

(defn table-editor-impl
  "return a function which acts as a table-view editor. Arguments: 
  example: (table-editor
             (fn [method component colname & [args]]
               (condp = method ;; one of :create, value or edit, component is nil for :create
                 :create (let [{d :data, [x y w h] :bounds} args]
                           (text-input :text d :preferred-width w))
                 :value (.getText component)
                 :edit (.selectAll component))))"
  [editor-fn]
  (let [edit-state (atom {:tv nil :row-idx nil :col-idx nil :component nil :popup nil})]
    ;; implemented after the pivot table-view-cell-editor, but with private vars held in an atom-map
    (fn table-view-row-editor
      ([k] (condp = k
             :cancel (if (:popup @edit-state)
                       (.close (:popup @edit-state))
                       (throwf "Not editing."))                       
             :editing? (boolean (:tv @edit-state))
             :save (let [{:keys [tv row-idx col-idx popup component]} @edit-state]
                     (when (nil? tv) (throwf "Not editing."))
                     (let [td (-> tv .getTableData)
                           colname (-> tv .getColumns (.get col-idx) .getName)
                           rowdata (->  td (.get row-idx))
                           value (.get rowdata colname)]
		       ;;(println "saving value: " (value-f component))
                       (.put rowdata colname (editor-fn :value component colname));;(.getText component))
                       (if (nil? (.getComparator td))
			 ;; for non-sorted tables:
                         (.update td row-idx, rowdata)
			 ;; sorted tables:
                         (let [copied-tv tv]
                           (.remove td row-idx 1)
                           (.add td rowdata)
                           (let [new-row-idx (.indexOf td rowdata)]
                             (.setSelectedIndex copied-tv new-row-idx)
                             (.scrollAreaToVisible copied-tv (.getRowBounds tv new-row-idx)))))))))
      ([current-tv, current-row-idx, current-col-idx]
         (reset! edit-state {:tv current-tv
                             :row-idx current-row-idx
                             :col-idx current-col-idx})
         (let [{:keys [tv row-idx col-idx]} @edit-state
               colname (-> tv .getColumns (.get col-idx) .getName)
               rowdata (->  tv .getTableData (.get row-idx))
               celldata (.get rowdata colname)
               cell-bounds (let [cb (.getCellBounds tv row-idx col-idx)]
                             (.scrollAreaToVisible tv cb)
                             (.getVisibleArea tv cb))
               ;; create the component
               edit-component (editor-fn :create nil colname
                                         {:rowdata rowdata
                                          :data celldata
                                          :bounds (get-bounds cell-bounds)
                                          :editor table-view-row-editor})
               ;; create the (popup) window
               edit-window (window :auxilliary true
                                   :location [(.x cell-bounds)
                                              (+ (.y cell-bounds) (/ (- (.height cell-bounds)
                                                                        (.getPreferredHeight edit-component -1) 2)))]
                                   edit-component)
               ;; listeners: require references to the other listeners
               display-mouse-handler
               (listener :container-mouse *
                         (fn [{m :method cont :container xy :int}]
                           (condp = m
                             :mouse-down (let [[x y] xy
                                               w (.getComponentAt cont x y)]
                                           (when-not (identical? edit-window w)
                                             ;; user clicked out of the bounds of this popup
                                             (table-view-row-editor :save))
                                           false)
                             :mouse-wheel true
                             false)))
               ;; cancel editing if any rows are modified while editing
               tv-row-listener (listener :table-view-row * (fn [_] (table-view-row-editor :cancel)))
               ;;  cancel editing on any table-data change
               tv-listener (listener :table-view *
                                     (fn [{m :method :as args}]
				       (println "table-view-listener" args)
                                       (when (#{:table-data-changed
                                                :table-view-row-editor-changed}
                                              m)
                                         (table-view-row-editor :cancel))))
               ;; cancel on ESCAPE, save on ENTER
               key-listener (listener :component-key *
                                      (fn [{m :method keycode :int :as args}]
					;;(println "key-listener:" (:method args) (:int args))
                                        (when (= m :key-pressed) 
                                          (condp = keycode
                                            Keyboard$KeyCode/ESCAPE (table-view-row-editor :cancel)
                                            Keyboard$KeyCode/ENTER (table-view-row-editor :save)
					    nil))
                                        false))
               ;; popup window-listener: edit initialization & finalization
               popup-state-handler
               (listener :window-state *
                         (fn [{:keys [method, window] :as args}]
                           (condp = method
                             :window-opened (let [disp (.getDisplay window)]
                                              (add-listener disp display-mouse-handler)
                                              (add-listener tv tv-listener)
                                              (add-listener tv tv-row-listener))
                             :window-closed (let [disp (:display args)]
                                              (remove-listener disp display-mouse-handler)
                                              (remove-listener tv tv-listener)
                                              (remove-listener tv tv-row-listener)
                                              (.requestFocus tv)
                                              (reset! edit-state {}))
			     :preview-window-open Vote/APPROVE
			     :preview-window-close Vote/APPROVE
			     nil)))]
           (swap! edit-state assoc :component edit-component :popup edit-window)
           (add-listener edit-component key-listener)
           (add-listener edit-window popup-state-handler)
           (.open edit-window (.getWindow tv))
           (editor-fn :edit edit-component colname);; eg for (.selectAll textinput)           
           (.requestFocus edit-component))))))


(defn listbox-editor-fn
  "a table-editor-fn wich opens a list-button with (keys m) choices upon edit
  and stores (get (f rowdata) the-selected-key) as data into the table after the listbox
  has been closed.
  (f must return a hashmap of displayed-values to stored-values)"
  [f]
  ;; needs internal state: the row to set the new index to
  (let [rowdata-atom (atom nil)
        m (atom nil)]
    (fn [method component colname & [arg-map]]
      (condp = method
        :create (let [{d :data [x y w h] :bounds rowdata :rowdata} arg-map
                      _ (reset! m (f rowdata))
                      l (list-button :data (if-let [e (first (filter #(= (val %) d) @m))]
                                             (key e)
                                             "")
                                     :preferred-width w
                                     :list-data (make-list (keys @m)))]
                  (reset! rowdata-atom rowdata)
                  (add-listener l (listener :list-button-selection * 
                                            (fn [_]
                                              ;; set the table-value "by-hand" because the 
                                              ;; editors cancel-when-clicked-outside event fires
                                              ;; before this components mouse-clicked-and-set-selected-item
                                              ;; event
                                              (.put rowdata colname (get @m (.getSelectedItem l))))))
                  l)
        :value (.get @rowdata-atom colname) ;; let the list-button-selection event set this column
        :edit nil))))

(defn named-table-editor
  "return a table-editor which fires a per-column custom table-editor-function
   on each invocation. Defaults to a text-input."
  [table-editor-fn-map]
  (let [default-editor;; text input
        (fn [method component colname & [arg-map]]
          (condp = method;; one of :create, value or edit, component is nil for :create
            :create (let [{d :data, [x y w h] :bounds} arg-map]
                      (text-input :text d :preferred-width w))
            :value (.getText component)
            :edit (.selectAll component)))]
    (table-editor-impl (fn [method component name & [arg-map]]
                         ((or (get table-editor-fn-map name default-editor) default-editor)
                          method component name arg-map)))))

;; table-view-cell-editor

(import '(org.apache.pivot.wtk TableView$CellRenderer
                               Label))

(defn table-view-lookup-renderer
  "like a plain cell renderer but looks up the table
  value in (f rowdata) (f returns a map) to find a value to display."
  [f]
  (fn ([] (label :styles {:vertical-alignment :center
                          :padding [2 2 2 2]}))
    ([{:keys [component, value, table-view, column, row-selected, row-highlighted row-disabled]}]
       (let [tv-styles (.getStyles table-view)
             color (.get tv-styles
                         (if (and (.isEnabled table-view) (not row-disabled))
                           (if row-selected 
                             (if (.isFocused table-view)
                               "selectionColor"
                               "inactiveSelectionColor")
                             "color")
                           "disabledColor"))]
         ;;(.setText component (str (get m (when value (.get value (.getName column))) value)))
         (.setText component (str (get (f value) (when value (.get value (.getName column))) value)))))))







