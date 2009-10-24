;   Copyright (c) 2009, Erik Soehnel All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns hoeck.pivot.tools
  (:use clojure.contrib.pprint
        clojure.contrib.except

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
                      Frame :application-blue
                      Button :ui-button
                      Accordion :ui-accordion
                      BoxPane :layout
                      Checkbox :ui-check-box
                      ScrollPane :ui-scroll-pane
                      RadioButton :ui-radio-button
                      TreeView :node
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
                      :question :question}]
    (zipmap (keys icon-mapping)
            (map #(str "hoeck/pivot/icons/" (name %) ".png") (vals icon-mapping)))))

(defn inspector-tree [c]    
  (let [p (get-properties c)
        components (concat (:components p) (filter #(isa? (type %) Component) (vals p)))
        text (str (.getSimpleName (type c)) " " (-> p :user :name))
        icon (or (inspector-icon-mapping (type c))
                 (inspector-icon-mapping (when-let [t (type c)] (.getSuperclass t)))
                 (inspector-icon-mapping :question))]
    (if (not (empty? components))
      (tree-branch :text text :icon icon
                   :nodes (map inspector-tree
                               components))
      (tree-node :text text :icon icon))))

(defn component-inspector
  "open a component inspector in frame in the current display"  
  ([display] (component-inspector display display))
  ([display root-component]
     (let [inspector-click-listener (listener :component-mouse-button * 
                                              #(do (println (:int %)) true))
           disp display
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
       (add-listener display inspector-click-listener)
       (.open (frame :self inspector-frame :title "Inspector") disp))))

