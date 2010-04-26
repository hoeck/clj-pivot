;   Copyright (c) 2009, Erik Soehnel All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns hoeck.pivot.tools
  (:use clojure.contrib.except
	hoeck.pivot.datastructures
        hoeck.pivot.components
        hoeck.pivot.listeners)
  (:require [hoeck.pivot.Application :as app])
  (:import (org.apache.pivot.wtk DesktopApplicationContext Application Display
                                 ;; containers
				 Container
                                 Window
                                 Form Form$Section
                                 SplitPane SplitPane$Region
                                 TabPane Accordion BoxPane Border
				 ScrollPane ScrollPane$ScrollBarPolicy Panorama
                                 StackPane Frame Viewport TablePane Panel FlowPane CardPane
                                 MovieView
				 ;; buttons
				 Button ButtonGroup PushButton RadioButton
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
                                 Menu MenuBar MenuButton Menu$Item MenuPopup
                                 ;; tables
                                 TableView TableViewHeader
				 TableView$SelectMode TableView$Column
				 TableView$CellRenderer TableView$RowEditor
                                 ;; enums, structs
                                 Orientation SortDirection Insets
				 Dimensions VerticalAlignment HorizontalAlignment)
           (org.apache.pivot.wtk DesktopApplicationContext)
	   (org.apache.pivot.wtkx WTKXSerializer)
           (org.apache.pivot.collections Dictionary)
	   (java.net URL)))

;; inspector

(def inspector-icon-mapping
  (let [icon-mapping {Window :application-blue
                      Frame :application
                      Button :ui-button
                      Accordion :ui-accordion
                      BoxPane :layout
                      Checkbox :ui-check-box
                      ScrollPane :ui-scroll-pane
                      RadioButton :ui-radio-button
                      TreeView :application-tree
                      ListButton :ui-combo-box
                      ListView :ui-list-box
                      TableView :table
                      Spinner :ui-spin
                      TabPane :ui-tab-content
                      TextInput :ui-text-field
                      Slider :ui-slider
                      Label :ui-label
                      CardPane :layers-stack-arrange
                      StackPane :layers-stack
                      SplitPane :ui-splitter
                      CalendarButton :calendar-month
                      Border :selection
                      MovieView :layer-resize-actual
                      Form :application-form
                      TablePane :application-split-tile
                      :question :question}]
    (zipmap (keys icon-mapping)
            (map #(str "hoeck/pivot/icons/" (name %) ".png") (vals icon-mapping)))))

(defn inspector-tree
  "given a pivot component, return a tree of (the-component & contained-components)
  (the model)"
  [c]
  (let [components (:components (get-properties c))]
    (if (not (empty? components))
      (cons c (map inspector-tree
                   components))
      (list c))))

(defn inspector-tree-nodes
  "given a component tree, return the same tree as tree-branches and tree-nodes."
  [[c & components]]
  (let [p (get-properties c)
        text (str (.getSimpleName (type c)) " " (-> p :user :name))
        icon (or (inspector-icon-mapping (type c))
                 (inspector-icon-mapping (when-let [t (type c)] (.getSuperclass t)))
                 (inspector-icon-mapping :question))]
    (if (not (empty? components))
      (tree-branch :text text 
                   :icon icon
                   :nodes (map inspector-tree-nodes components))
      (tree-node :text text 
                 :icon icon))))

(defn get-component-from-tree-path
  "given the component tree and a path, return a component"
  [root path]
  (if (seq path)
    (get-component-from-tree-path (nth (next root) (first path))
                                  (next path))
    (first root)))

(defn property-table
  "generates table-data from the properties of a component o."
  [o]
  (let [props (get-properties o)
        docs (or (get-all-property-defs o) {})]
    (map (fn [[k v]] {'key k 'val v 'doc (:doc (docs k))})
         props)))

(def selected-component (atom nil))
(defn inspector-tree-view-listener
  "listens to inspector treeview selections and updates details
  for the selected component"
  [inspector-detail components-tree argmap]
  (if (#{:selected-paths-changed :selected-path-added} (:method argmap))    
    (let [tv (:tree-view argmap)]
      (when-let [path (seq (.getSelectedPath tv))]
        (let [c (get-component-from-tree-path components-tree path)]
          (reset! selected-component c)
          (when c (set-properties inspector-detail {:data (make-dictionary-list (property-table c))}))
          true)))
    false))

(defn inspector-tree-key-listener [tree-view argmap]
  (println (:int argmap))
  false)

(def current-inspector-frame (atom nil))
(defn component-inspector
  "open a component inspector in frame in the current display, using display as the component root." 
  ([display] (reset! current-inspector-frame
		     (component-inspector display display @current-inspector-frame)))
  ([display root-element container-frame] ;; the container to display in
     (let [disp display
           components-tree (inspector-tree root-element)
           inspector-tv (tree-view :preferred-width [* * *]
                                   :data (inspector-tree-nodes components-tree))
           inspector-detail (table-view :preferred-width [100 * *]
                                        :preferred-height [100 * *]
                                        (table-view-column :header-data "Property" :name "key")
                                        (table-view-column :header-data "Value" :name "val")
                                        (table-view-column :header-data "Documentation" :name "doc" :width 300))
           inspector-component (boxpane
                                :orientation :vert
                                :user {:name 'component-listener-toplevel-box}
                                :styles {:fill true}
                                (border
                                 (splitpane :preferred-width [300 * *]
                                            :preferred-height [500 * *]
                                            :user {:name 'my-splitpane}
                                            :top-left (scrollpane :preferred-width [100 * *]
                                                                  :preferred-height [50 * *]
                                                                  :view inspector-tv)
                                            :orientation :horiz
                                            :bottom-right (scrollpane :preferred-width [100 * *]
                                                                      :column-header (table-view-header :table-view inspector-detail)
                                                                      :view inspector-detail)
                                            :primary-region :top-left
                                            :split-ratio 0.6)))]
       (add-listener inspector-tv (listener :tree-view-selection
                                              #(inspector-tree-view-listener inspector-detail components-tree %)))
       ;;(add-listener inspector-tv (listener :component-key * #(inspector-tree-key-listener inspector-tv %)))
       (if container-frame
	 (do (.moveToFront container-frame)
	     (frame :self container-frame :content inspector-component))
         (let [f (frame :preferred-width [600 * *]
                        :title "Inspector"
                        :user {:name `inspector-frame}
                        :content inspector-component)]
           (do (.open f disp)
               f))))))

(comment
  
  ;; invoke the inspector
  (reset! current-inspector-frame nil)
  (hoeck.pivot/pivot-invoke #(component-inspector (@hoeck.pivot/appstate :display)))  

  (frame :self @current-inspector-frame :preferred-width [800 * *] :preferred-height 550)

  (.moveToFront @current-inspector-frame)
  )
