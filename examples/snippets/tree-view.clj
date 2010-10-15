
;; snippets to be used on the repl or within the example applet
(ns demo)

(defmacro show [& body]
  `(pivot/disp-> (pivot/show (do ~@body))))

;; ---title---
;; Tree-View Tutorial
;; ---description---
;; learn how to use pivot tree-views

;; ---title---
;; (0) setup
;; ---description---
;; set up aliases to the required clj-pivot namespaces
;; ---code---
(require '[hoeck.pivot :as pivot])
(require '[hoeck.pivot.components :as c])
(require '[hoeck.pivot.datastructures :as ds])

;; ---title---
;; (1) tree-view with static literal data
;; ---description---
;;
;; |-- a
;; |   |-- 1
;; |   `-- 2
;; `-- b
;;     |-- 3
;;     `-- 4
;;
;; ---code---

(show (c/tree-view
       :data (ds/make-list [(c/tree-branch :text "a"
                                           (c/tree-node :text "1")
                                           (c/tree-node :text "2"))
                            (org.apache.pivot.wtk.content.TreeNode. "foo")
                            (c/tree-branch :text "b"
                                           (c/tree-node :text "3")
                                           (c/tree-node :text "4"))])))

;; ---title---
;; (2) Namespace Inspector
;; ---description---
;;
;; Make the current clojure environment tree-view browsable
;; Display each namespace in a branch with functions and vars
;; Display Functions docs as nodes.
;;
;; ---code---

(defn create-ns-node [var]
  (let [{:keys [doc name line arglists]} (meta var)]
    (if (and (bound? var) (fn? (deref var)))
      (c/tree-branch :text (str "fn: " name)
                     :nodes [(c/tree-node :text doc)
                             (c/tree-node :text (str "line: " line))
                             (c/tree-branch :text (format "(%s) arglists" (count arglists))
                                            :nodes (map #(c/tree-node :text (str %)) arglists))])
      (c/tree-branch :text (str "var:" name)
                     :nodes [(c/tree-node :text (str "line: " line))]))))

(defn create-ns-branch [ns]
  (c/tree-branch :text (ns-name ns)
                 :nodes (->> (ns-publics ns)
                             vals
                             (map create-ns-node))))

(defn create-tv-data []
  (ds/make-list (map create-ns-branch (all-ns))))

(show (c/tree-view
       :data (create-tv-data)))


;; ---title---
;; (3) Namespace Inspector with scrollpane
;; ---description---
;;
;; Place the tree-view inside a scrollpane
;;
;; ---code---

(show (c/scrollpane
       :view (c/tree-view
              :data (create-tv-data))))

