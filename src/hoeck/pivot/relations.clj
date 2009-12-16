
(ns hoeck.pivot.relations
  (:use clojure.contrib.except
         ;; todo: refactor update functions into their own ns: hoeck.rel.update
        hoeck.pivot.components
        hoeck.pivot.listeners)
  (:require [clojure.set :as set]
            [hoeck.rel.update :as rel-update])
  (:import (org.apache.pivot.collections List ArrayList)
           (org.apache.pivot.collections.concurrent SynchronizedList)
           (org.apache.pivot.util ListenerList)))

(defn pivot-list 
  "Given a relation, return a pivot List of its tuples."
  ;; todo: handle sorted relations so that they return a
  ;;       sorted List and have the comparator already set
  [R]
  (ArrayList. (into-array R)))

(defn relation-model
  "Returns the same relation R wrapped in an atom and connected to a pivot
  List of tuples (available with get-pivot-model).
  `Connected' in the sense that changes to the pivot list will be applied
  (via hoeck.rel.update insert, delete and update) to R.
  To mutate R, use the model-(update, delete, insert) functions."
  [R]
  ;; todo: throw errors if an identical item is inserted
  (let [;; convert R to pivot
        piv (pivot-list R)
        R (atom R :meta {::pivot-model piv})
	ll (list-listener {idx :int l :list :as args}
             :item-inserted (swap! R rel-update/insert (.get l idx))
             :items-removed (apply swap! R #(apply rel-update/delete % (:sequence args)))
             :item-updated (swap! R rel-update/update (:object args) (.get l idx))
             :list-cleared (swap! R #(apply rel-update/delete % %))
             :comparator-changed nil)]
    (add-listener piv ll)
    R))

(defn get-pivot-model
  "Return the `thing's existing pivot model"
  [thing]
  (-> thing meta ::pivot-model))

(defn update ;; modify the model, make the view updateable to allow tv-row-editors
  "Update specific keys inside a relation model or a pivot-view thereof.
  For the latter, one may also specify an index of the view instead of a tuple.
  Always returns o."
  ;; todo: throw exceptions
  [o tuple-or-idx & key-val-pairs-or-hashmap]
  (cond (isa? o List) (let [a key-val-pairs-or-hashmap
                            t (if (map? (first a)) (first a) (apply hash-map a))
                            idx (if (integer? tuple-or-idx)
                                  tuple-or-idx
                                  (.indexOf o tuple-or-idx))]
                        (.update o idx (merge (.get o idx) t)))
        :else (apply update (get-pivot-model o)
                     tuple-or-idx
                     key-val-pairs-or-hashmap))
  o)

(defn insert
  "delete from a relation model"
  [r & new-tuples]
  (let [m (get-pivot-model r)]
    (doseq [t new-tuples]
      (.add m t))
    r))

(defn delete
  "insert into a relation model"
  [r & tuples]
  (let [m (get-pivot-model r)]
    (doseq [t tuples]
      (.remove m t))
    r))
