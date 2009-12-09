
(ns hoeck.pivot.amplify
  (:use hoeck.pivot
        hoeck.pivot.components
        hoeck.pivot.listeners
        hoeck.pivot.datastructures
        hoeck.pivot.editors
        ;;hoeck.rel
        ))

;; configurable table-view: relation-view

(defn make-table-view-model
  "Given a relation, make a table-model (a pivot list of dictionaries) and make each
  dictionary fireing listchanged events when its contents are changed."
  [R] ;; or list-of-dictionaries
  (let [l (make-dictionary-list R)
        ;; fires list update events on updates of single tuples
        dl (listener :map * (fn [{method :method m :map}]
                              (let [i (.indexOf l m)]
                                (-> l .getListListeners (.itemUpdated l i m)))))]
    (doseq [m l] (add-listener m dl))
    l))

(defn relation-model
  "Given a clojure relation, return this relation in an atom and a pivot dictionary list.
  Whenever the pivot list is changed, update the clojure-relation inside the atom."
  [R]
  ;; todo: throw errors if an identical item is inserted or if primary keys a altered
  (let [ ;; convert R to pivot
        piv (make-table-view-model R)
        ;; mutable R
        R (atom R)
	lf (fn listener-fn [{idx :int m :method :as args}]
	     (condp = m
	       :item-inserted (swap! R conj (dictionary->hashmap (.get piv idx)))
	       :items-removed (apply swap! R disj (map dictionary->hashmap (:sequence args)))
	       :item-updated (swap! R conj (dictionary->hashmap (.get piv idx)))
	       :list-cleared (swap! R empty)
	       :comparator-changed nil))]
    (add-listener piv (listener :list * lf))
    [R piv]))

(defn column
  "define a column in a relation-view"
  [field & args]
  (let [args (apply hash-map args)
        [field-type & ft-args] (:type args)]
    (println args)
    (merge {:field field
            :header (get args :header (str field))}
           ;; special renderer and editor combinations
           (condp = field-type
             :list-button (let [a-b (first ft-args)
                                b-a (zipmap (vals a-b) (keys a-b))]
                            {:renderer (table-view-lookup-renderer (fn [_] a-b))
                             :editor (listbox-editor-fn (fn [_] b-a))})
             :list-button-f (let [[display-fn listbox-fn] ft-args]
                              {:renderer (table-view-lookup-renderer display-fn)
                               :editor (listbox-editor-fn listbox-fn)})
             {:renderer :string :editor nil}))))

(defn relation-view
  "Return a component which shows tuples of a relation in a table-view."
  [R & args]
  (let [colnames (map str (map :field args))
        cols (map #(table-view-column :header-data %1
                                      :name %2
                                      :cell-renderer %3)
                  (map :header args)
                  colnames
                  (map :renderer args))
	[model, piv] (relation-model R)
	tv (apply table-view
                  :data piv
                  :editor (let [editor-map (zipmap colnames (map :editor args))]
                            (named-table-editor editor-map))
                  cols)]
    {:model model,
     :component (scrollpane :preferred-width [100 * *]
                            :column-header (table-view-header :table-view tv)
                            :view tv)
     :table-view tv}))

(comment ;; example
  (relation-view #{{:a 1} {:a 2} {:a 3}}
                 (column :a :header "the A" :type [:list-button {1 "a",2 "b", 3 "c"}]))
  )


