

;; an applet to provide a simple environment to click
;; through sample applications

(ns hoeck.pivot.examples.code-browser
  (:require [hoeck.pivot.components :as c]
            [hoeck.pivot.listeners :as l]
            [hoeck.pivot :as pivot]))

;;;;;;;;;;;;;;;

(def example-resources ["celsius.clj"
                        "forms.clj"
                        "tooltips.clj"])

(defn slurp-stream
  "Reads the stream is by f using the encoding enc into a string
  and returns it."
  ([is]
     (with-open [r (->  is
                        java.io.InputStreamReader.
                        java.io.BufferedReader.)]
    (let [sb (StringBuilder.)]
      (loop [c (.read r)]
        (if (neg? c)
          (str sb)
          (do
            (.append sb (char c))
            (recur (.read r)))))))))

(def section-regex #"^;; ---(.*)---")

(defn strip-semicolons [s]
  (->> (.split #"\n" s)
       (map #(re-matches #"^(?:;+ ){0,1}(.*)" %))
       (map second)
       (interpose \newline)
       (apply str)))

(defn read-example [is]
  (->> (.split #"\n" (slurp-stream is))
       (partition-by (partial re-matches section-regex))
       (drop 1)
       (partition 2)
       (map (fn [[[k] v]]
              [(-> (re-matches section-regex  k) second (.toLowerCase) keyword)
               (->> v
                    (interpose \newline)
                    (apply str))]))
       (partition-by #(= (first %) :title))
       (partition 2)
       (map #(apply concat %))
       (map #(into {} %))
       (map (fn [{:keys {title, description} :as m}]
              (-> m (update-in [:title] strip-semicolons)
                    (update-in [:description] strip-semicolons))))))

(defn read-examples []
  (->> example-resources
       (map #(ClassLoader/getSystemResourceAsStream %))
       (remove nil?)
       (map #(with-open [is %]
               (read-example is)))
       (map (fn [[{:keys [title, description]} & sections]]
              {:title title
               :description description
               :sections sections}))
       doall))

(def tutorials (read-examples))


;; (pivot/start)

(defn eval-area []
  (c/table-pane
   :styles {:vertical-spacing 2}
   :cols [[1]]
   [(c/boxpane :styles {:fill true}
               :orientation :horiz
               (c/separator :heading "Description")
               (c/text-area :editable false
                            :text ""
                            :user-name ::description))]
   (c/table-pane-row
    :height [1]
    (c/border :title " code "
              (c/scrollpane
               :vert-scrollbar-policy :fill-to-capacity
               :horiz-scrollbar-policy :fill-to-capacity
               :view (c/text-area :text "(c/push-button :data \"foo\" :user {:name :foo :tooltip \"blah\nblah\"})"
                                  :user-name ::source))))
   [(c/table-pane :cols [-1 [10] 100]
                  [(c/label :styles {:vertical-alignment :center} "result:")
                   (c/label :styles {:vertical-alignment :center} :text "" :user-name ::result)
                   (c/push-button :data "eval"
                                  :preferred-width [* 100 *]
                                  :user-name ::eval-button)])]))

(defn load-code [r src descr]
  (-> r
      (c/find-component ::demo-component-border)
      (c/set-property :content (c/filler)))
  (-> r
      (c/find-component ::source)
      (c/set-property :text src))
  (-> r
      (c/find-component ::description)
      (c/set-property :text descr)))

(defn tutorial-pane []
  (c/splitpane
   :orientation :vert
   :top (c/border :user-name ::demo-component-border
                  :title " show "
                  :styles {:padding [2 2 2 2]})
   :bottom (eval-area)))

(defn show-component [thing]
  (c/set-property (c/find-component pivot/display ::demo-component-border)
                  :content thing)
  nil)

(defn show-result [r]
  (c/set-property (c/find-component pivot/display ::result)
                  :text (pr-str r)))

(defn eval-action [c]
  (let [src (-> c
                (c/find-component ::source)
                (c/get-property :text))
        r (try
           (load-string (str "(in-ns 'demo) (def show hoeck.pivot.examples.code-browser/show-component) " src))
           (catch Exception e e))]
    (show-result r)
    (when (c/component? r) (show-component r))))

(defn tut-menu []
  (apply c/accordion
         (map (fn [{:keys [title, description, sections] :as foo}]
                (c/accordion-panel :label title
                                   (apply c/boxpane :orientation :vert
                                          (map (fn [{:keys [title code description]}]
                                                 (c/link-button :data title
                                                                :action #(pivot/disp-> (load-code code (str description)))))
                                               sections))))
          tutorials)))

(defn gui []
  (let [c (c/table-pane
           :styles {:padding [2 2 2 2]}
           :cols [[10]]
           (c/table-pane-row
            :height [1]
            (c/splitpane
             :split-ratio 0.2
             :left (tut-menu)
             :right (tutorial-pane))))]
    (-> (c/find-component c ::eval-button)
        (c/set-property :action #(eval-action c)))
    c))

(defn demo-ns-setup []
  (in-ns 'demo)
  (clojure.core/refer 'clojure.core)
  (require '[hoeck.pivot.components :as c]
           '[hoeck.pivot.listeners :as l]
           '[hoeck.pivot :as pivot])
  (in-ns 'hoeck.pivot.examples.code-browser))

(defn launch []
  (demo-ns-setup)
  (pivot/disp-> (pivot/show (gui))))

(launch)

;; to launch from the repl: (do (pivot/start) (launch))
;; closing the then opened window will close the repl too
