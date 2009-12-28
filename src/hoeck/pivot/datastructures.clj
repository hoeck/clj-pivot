;   Copyright (c) 2009, Erik Soehnel All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; interfacing clojure with pivot datastructures through proxying and generation
(ns hoeck.pivot.datastructures
  (:use clojure.contrib.except)
  (:import (org.apache.pivot.collections List Map Dictionary HashMap  LinkedList)))


;; tools

(defn clj-key
  "given a string, generate a clojure keyword or symbol based on the first character (`:' or not)."
  [k]
  (if (string? k)
    (if (.startsWith k ":") (keyword (.substring k 1)) (symbol k))
    k))

;; clojure to pivot ctors

(defn make-list
  "Make a pivot List from a clojure sequence."
  [s]
  (let [l (LinkedList.)]
    (doseq [elem s]
      (.add l elem))
    l))

(defn make-dictionary
  "Make a pivot Dictionary (using a pivot HashMap) from a clojure hashmap. Convert keys and symbols to strings
  using `str'."
  [m]
  (if (empty? m)
    (HashMap.)
    (let [dict (HashMap. (count m))]
      (doseq [[k v] m] (.put dict (str k) v))
      dict)))

(defn remove-nils
  "given a clojure hashmap, remove all entries where the value is nil.
  Useful when loading tuples in forms. Text-inputs throw an error on nil
  values for :text."
  [m]
  (into {} (filter val m)))

(defn make-dictionary-list
  "Create a list of dictionarys from a clojure seq of hashmaps (e.g. a relation). Convert symbol and keyword keys
  to string keys."
  [R] ;; or list-of-dictionaries
  (make-list (map make-dictionary R)))

(defn dictionary->hashmap
  "Return a clojure hashmap from a pivot dictionary.
  Translate keys from Strings to keywords/symbols (based on a leading colon)."
  [d]
  (reduce (fn [m k]
	    (assoc m (clj-key k) (.get d k)))
	  {} (seq d)))

(defn dictionary->hashmap-str
  "Return a clojure hashmap from a pivot dictionary."
  [d]
  (reduce (fn [m k]
	    (assoc m k (.get d k)))
	  {} (seq d)))

(defn dictionary-list->relation
  [l]
  (set (map dictionary->hashmap l)))

(comment
  (dictionary-list->relation (make-dictionary-list #{{:foo 'bar :number 9}
						     {:foo 'baz :number 0}
						     {:foo 'xxx :number 2}})))

;; proxy implementations

(defn immutable-dictionary
  "Wraps a clojure hashmap in a org.apache.pivot.collections.Dictionary.
  For use as data dictionaries (in tables etc.), keys are treatened as Strings and are translated
  to keys or symbols before looking them up in the clojure map.
  Implements only immutable parts of the Interface, otherwise throw an exception."
  [m]  
  (proxy [Dictionary] []
    ;;boolean containsKey(K key) Tests the existence of a key in the dictionary.
    (containsKey [k] (contains? m (clj-key k)))
    ;;V get(K key) Retrieves the value for the given key.
    (get [k] (get m (clj-key k)))
    ;;boolean isEmpty() Tests the emptiness of the dictionary.
    (isEmpty [] (empty? m))
    ;;V put(K key, V value) Sets the value of the given key, creating a new entry or replacing the existing value.
    (put [k v] (throwf "Immutable Dictionary, put not allowed"))
    ;;V remove(K key) Removes a key/value pair from the map.
    (remove [k] (throwf "Immutable Dictionary, remove not allowed"))))

(defn mutable-dictionary
  "Same as `ìmmutable-dictionary' but implement the mutable parts using an atom."
  [m]
  (let [m (atom m)]
    (proxy [Dictionary] []
      (containsKey [k] (contains? @m (clj-key k)))
      (get [k] (get @m (clj-key k)))
      (isEmpty [] (empty? @m))
      (put [k v] (swap! m assoc (clj-key k) v))
      (remove [k] (swap! m dissoc (clj-key k))))))

(defn- mutable-list-item-dictionary
  "Generates a dictionary from a hashmap at index in a given vector."
  [vec-ref comparator-ref index]
  ;; each change is a whole entry update in the parent list  
  (proxy [Dictionary] []
    (containsKey [k] (contains? (nth @vec-ref index) (clj-key k)))
    (get [k] (get (nth @vec-ref index) (clj-key k)))
    (isEmpty [] (empty? (nth @vec-ref index)))
    (put [k v]
	 (dosync (alter vec-ref assoc-in [index (clj-key k)] v)
		 (when @comparator-ref (ref-set vec-ref (vec (sort @comparator-ref @vec-ref)))))
	 nil)
    (remove [k]
	    (dosync (alter vec-ref assoc index (dissoc (nth @vec-ref index) (clj-key k)))))))

(defn dictionary-list
  "Given a ref to a vector of hashmaps, return an org.apache.pivot.collections.List implementation containing
  org.apache.pivot.collections.Dictionary-ies.
  Best used as a tableData model."
  [v]
  (let [comp (ref nil)]
    (proxy [List] []
      ;;int add(T item)
      (add [item] (dosync (alter v conj item) (count @v)))
      (clear [] (dosync (alter v (constantly (empty v)))) nil)
      (getLength [] (count @v))
      (getListListeners [] nil)  ;;ListenerList<ListListener<T>> getListListeners()
      (insert [item index]
	      (dosync (when @comp (throwf "Comparator set, cannot insert new items, use add instead."))
		      (ref-set v (vec (concat (subvec @v 0 index)
					      [item]
					      (subvec @v index (count @v))))))
	      nil)
      (remove ([item] (dosync (let [[before after] (split-with #(= item %) @v)]
				(ref-set v (concat before after)))))
	      ([index n]
		 (dosync (ref-set v (vec (concat (subvec @v 0 index)
						 (subvec @v (+ index n) (count @v)))))
			 (make-list (subvec @v index (+ index n))))))
      (setComparator [c] (dosync (ref-set comp c)
				 (ref-set v (vec (sort @comp @v)))))
      (getComparator [] @comp)
      (update [index item]
	      (dosync 
	       (alter v assoc index item)
	       (when @comp (ref-set v (vec (sort @comp @v))))))
      (get [index] 
	   ;; (nth @v index)
	   (mutable-list-item-dictionary v comp index))
      (indexOf [item] (first (filter #(= item %) @v)))
      (iterator [] (.iterator (map #(mutable-list-item-dictionary v comp %) (range 0 (count @v))))))))


