;; tableviews
(ns demo)

;; ---title---
;; Using Tableviews
;; ---description---
;; clj-pivot - table-views

;; ---title---
;; (0) setup
;; ---description---
;; Require components, listeners, pivot and table-view content implementations
;; ---code---
(require '[hoeck.pivot :as pivot])
(require '[hoeck.pivot.components :as c])
(require '[hoeck.pivot.listeners :as l])
(require '[hoeck.pivot.content.table-view :as content])

;; ---title---
;; (1) Displaying table-view data format 
;; ---description---
;; The data for a table-view is a seq of maps. Each map represents one row, the maps entries represent the column values.
;; This is the same format that clojure.core/resultset-seq and clojure.set are using for relational data.
;; Internally, the table view uses a pivot linked-list witch generates events for list modifications.
;; ---code---
(def table-view-data
     [{:foo 1 :bar true :baz "foo"}
      {:foo 2 :bar false :baz "bar"}
      {:foo 3 :bar true :baz "baz"}
      {:foo nil :bar nil :baz nil}])

;; ---title---
;; (2) Displaying the table-view
;; ---description---
;; Create a table view and describe the columns it should display.
;; Only four empty rows are displayes.
;; ---code---
(show (c/table-view
       :data table-view-data
       (c/table-view-column :name :foo)
       (c/table-view-column :name :bar)
       (c/table-view-column :name :baz)))


;; ---title---
;; (3) Defining data renderers
;; ---description---
;; Define a simple text renderer for each column.
;; ---code---
(show (c/table-view
       :data table-view-data
       (c/table-view-column :name :foo
                            :renderer (content/text-renderer))
       (c/table-view-column :name :bar
                            :renderer (content/text-renderer))
       (c/table-view-column :name :baz
                            :renderer (content/text-renderer))))


;; ---title---
;; (4) Table-view headers
;; ---description---
;; Embed the table-view inside a scrollpane and add column headers.
;; ---code---
(let [tv (c/table-view
         :data table-view-data
         (c/table-view-column :header-data "Foo"
                              :name :foo
                              :renderer (content/text-renderer))
         (c/table-view-column :header-data "Bar"
                              :name :bar
                              :renderer (content/text-renderer))
         (c/table-view-column :header-data "Baz"
                              :name :baz
                              :renderer (content/text-renderer)))]
  (show (c/scrollpane :column-header (c/table-view-header :table-view tv)
                      :view tv)))

;; ---title---
;; (5) Table-view editors
;; ---description---
;; Add an editor to editor column values. Edit values by double-clicking on a cell.
;; ---code---
(let [tv (c/table-view
          :data table-view-data
          :editor (content/text-input-editor)
         (c/table-view-column :header-data "Foo"
                              :name :foo
                              :renderer (content/text-renderer))
         (c/table-view-column :header-data "Bar"
                              :name :bar
                              :renderer (content/text-renderer))
         (c/table-view-column :header-data "Baz"
                              :name :baz
                              :renderer (content/text-renderer)))]
  (show (c/scrollpane :column-header (c/table-view-header :table-view tv)
                      :view tv)))


;; ---title---
;; (6) Table-view custom renderers
;; ---description---
;; Add a checkbox-renderer to the :bar column (which contains only boolean data).
;; ---code---
(let [tv (c/table-view
          :data table-view-data
          :editor (content/text-input-editor)
         (c/table-view-column :header-data "Foo"
                              :name :foo
                              :renderer (content/text-renderer))
         (c/table-view-column :header-data "Bar"
                              :name :bar
                              :renderer (content/checkbox-renderer))
         (c/table-view-column :header-data "Baz"
                              :name :baz
                              :renderer (content/text-renderer)))]
  (show (c/scrollpane :column-header (c/table-view-header :table-view tv)
                      :view tv)))


;; ---title---
;; (7) Table-view custom editing
;; ---description---
;; Add a listener to toggle the :bar checkboxes through a single click.
;; ---code---
(let [tv (c/table-view
          :data table-view-data
          :editor (content/text-input-editor)
         (c/table-view-column :header-data "Foo"
                              :name :foo
                              :renderer (content/text-renderer))
         (c/table-view-column :header-data "Bar"
                              :name :bar
                              :renderer (content/checkbox-renderer))
         (c/table-view-column :header-data "Baz"
                              :name :baz
                              :renderer (content/text-renderer)))]
  (l/add-listener
   tv
   (l/component-mouse-button-listener
    {[x y] :int c :component}
    :mouse-click (if (= 1 (.getColumnAt tv x))
                   (let [td (c/get-property tv :data)
                         row (.getRowAt tv y)
                         tup (.get td row)]
                     (.update td row (update-in tup [:bar] not))
                     true)
                   false)
    false))
  (show (c/scrollpane :column-header (c/table-view-header :table-view tv)
                      :view tv)))


;; ---title---
;; (8) Table-view custom editing part II
;; ---description---
;; Add a list-button editor to the last column.
;; ---code---
(let [tv (c/table-view
          :data table-view-data
          :editor (content/column-dispatch-editor ;; use different cell editors based on columnname 
                   :foo (content/text-input-editor)
                   :baz (content/list-button-editor [{:text "foo"}
                                                     {:text "bar"}
                                                     {:text "baz"}
                                                     {:text ""}]))
         (c/table-view-column :header-data "Foo"
                              :name :foo
                              :renderer (content/text-renderer))
         (c/table-view-column :header-data "Bar"
                              :name :bar
                              :renderer (content/checkbox-renderer))
         (c/table-view-column :header-data "Baz"
                              :name :baz
                              :renderer (content/text-renderer)))]
  (l/add-listener
   tv
   (l/component-mouse-button-listener
    {[x y] :int c :component}
    :mouse-click (if (= 1 (.getColumnAt tv x))
                   (let [td (c/get-property tv :data)
                         row (.getRowAt tv y)
                         tup (.get td row)]
                     (.update td row (update-in tup [:bar] not))
                     true)
                   false)
    false))
  (show (c/scrollpane :column-header (c/table-view-header :table-view tv)
                      :view tv)))

