

;; an applet to provide a simple environment to click
;; through sample applications

(ns hoeck.pivot.examples.applet
  (:require [hoeck.pivot.components :as c]
            [hoeck.pivot :as pivot]))

(def tutorials (atom []))

;; (pivot/start)

(defn eval-area []
  (c/table-pane
   :styles {:vertical-spacing 2}
   :cols [[10]]
   [(c/boxpane
     :orientation :vert
     :styles {:fill true}
      (c/separator :heading "Description")
      (c/label
       :styles {:wrap-text true}
       :text ""
       :user-name ::description))]
   (c/table-pane-row
    :height [10]
    (c/border :title " code "
              (c/text-area :text "(c/push-button :data \"foo\")"
                           :user-name ::source)))
   [(c/boxpane :orientation :horiz
               :styles {:horizontal-alignment :right}
               (c/push-button :data "eval"
                              :preferred-width [* 100 *]
                              :user-name ::eval-button))]))

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
   :styles {"useShadow" true}
   :orientation :vert
   :top (c/border :user-name ::demo-component-border
                  :title " result "
                  :styles {:padding [2 2 2 2]})
   :bottom (eval-area)))

(defn eval-action [c]
  (let [src (-> c
                (c/find-component ::source)
                (c/get-property :text))]
    (c/set-property
     (c/find-component c ::demo-component-border)
     :content
     (let [r (try ;;(eval (read-string (str "(binding [*ns* (find-ns " *ns* ")] " src " )")))
              (load-string (str "(in-ns 'demo)" src))
              (catch Exception e e))]
       (if (instance? org.apache.pivot.wtk.Component r)
         r
         (c/label r))))))

(defn tut-menu []
  (apply c/accordion
         (map (fn [{:keys [name, description, chapters] :as foo}]
                (c/accordion-panel :label name
                                   (apply c/boxpane :orientation :vert
                                          (map (fn [{:keys [name code description]}]
                                                 (c/link-button :data name
                                                                :action #(pivot/disp-> (load-code code (str description)))))
                                               chapters))))
          @tutorials)))

(defn gui []
  (let [c (c/table-pane
           :styles {:padding [2 2 2 2]}
           :cols [[10]]
           (c/table-pane-row
            :height [1]
            (c/splitpane
             :left (tut-menu)
             :right (tutorial-pane))))]
    (-> (c/find-component c ::eval-button)
        (c/set-property :action #(eval-action c)))
    c))

(defn demo-ns-setup []
  (in-ns 'demo)
  (clojure.core/refer 'clojure.core)
  (require '[hoeck.pivot.components :as c]
           '[hoeck.pivot :as pivot]))

(defn launch []
  (demo-ns-setup)
  (pivot/disp-> (pivot/show (gui))))


;;;;;;;;;;;;;;;,,

(defn code [& strings]
  (apply str (interpose \newline strings)))

(def converter {:name "Celsius Converter"
                :description "Uses the Celsius converter example from clojure.org."
                :chapters [{:name "Components"
                            :code (code "(let [ ;; we need: a textinput"
                                        "      celsius-input (c/text-input :text \"23\")"
                                        "      ;; a button"
                                        "      convert-button (c/push-button :data \"Convert\")"
                                        "      ;; two labels"
                                        "      celsius-label (c/label \"Celsius\")"
                                        "      fahrenheit-label (c/label \"73.4 Fahrenheit\")]"
                                        "  ;; to be able to see them all at once on our display,"
                                        "  ;; we wrap them in a container (a boxpane in this case)"
                                        "  ;; and stack them vertically"
                                        "  (c/boxpane"
                                        "   :orientation :vert"
                                        "   celsius-input"
                                        "   convert-button"
                                        "   celsius-label"
                                        "   fahrenheit-label))")}
                           {:name "Convert function"
                            :code (code "(defn convert [input-str]"
                                        "  (let [c (try (Double/parseDouble input-str) (catch NumberFormatException e nil))]"
                                        "    (str (if c (+ 32 (* 1.8 c)) \"-\") \" Fahrenheit\")))"
                                        " "
                                        ";; try it:"
                                        "(convert \"23\")")}
                           {:name "Adding functionality"
                            :code (code ";; next, we wire up the convert function to the convert button:"
                                        ";; the push-button has an :action arg, which is a function without"
                                        ";; arguments, called when someone clicks the button"
                                        "(let [celsius-input (c/text-input :text \"23\")"
                                        "        celsius-label (c/label \"Celsius\")"
                                        "        fahrenheit-label (c/label \"73.4 Fahrenheit\")"
                                        "        convert-button (c/push-button :data \"Convert\""
                                        "                                       :action #(c/set-property"
                                        "                                                 ;; will set the :text -property"
                                        "                                                 ;; of fahrenheit-label"
                                        "                                                 fahrenheit-label :text"
                                        "                                                 (convert (c/get-property"
                                        "                                                           celsius-input"
                                        "                                                           :text))))]"
                                        "    (c/boxpane"
                                        "     :orientation :vert"
                                        "     celsius-input"
                                        "     convert-button"
                                        "     celsius-label"
                                        "     fahrenheit-label))")}
                           {:name "Layout finish"
                            :code (code ";; so the application works as excpected, but we have to change the layout"
                                        ";; to our requirements"
                                        "(let [celsius-input (c/text-input :text \"23\")"
                                        "        celsius-label (c/label \"Celsius\")"
                                        "        fahrenheit-label (c/label \"73.4 Fahrenheit\")"
                                        "        convert-button (c/push-button :data \"Convert\""
                                        "                                     :action #(c/set-property"
                                        "                                               fahrenheit-label :text"
                                        "                                               (convert (-> celsius-input"
                                        "                                                            (c/get-property :text)))))]"
                                        "    ;; we use a table-pane"
                                        "    ;; this is the most versatile container to achieve various kinds"
                                        "    ;; of tabular layouts"
                                        "    (c/table-pane"
                                        "     ;; we have 2 columns, each have a relative width of 1,"
                                        "     ;; each col will get 50% of the available space"
                                        "     :cols [[1] [1]]"
                                        "     ;; now we style the table pane to get some custom spacings"
                                        "     ;; to see all available styles for eg. a table-pane, type"
                                        "     ;; (pprint-styles table-pane) the the repl"
                                        "     :styles {:vertical-spacing 2"
                                        "              :horizontal-spacing 2"
                                        "              :padding [2 2 2 2]}"
                                        "     ;; 2 rows of components"
                                        "     (c/table-pane-row celsius-input celsius-label)"
                                        "     (c/table-pane-row convert-button fahrenheit-label)))")}]})

(def example-form {:name "Form"
                   :description "How to lay out forms using a table pane."
                   :chapters [{:name "Required components"
                               :description "Shows all components required for the form: label, separator and text-input."
                               :code (code "(c/boxpane :orientation :vert"
                                           "           :styles {:fill true}"
                                           "           (c/label :text \"Label\")"
                                           "           (c/separator :heading \"Separator\")"
                                           "           (c/text-input :prompt \"text-input\"))")}
                              {:name "Boxpane container"
                               :description "Tries to layout a form using a boxpane and shows its limitiations."
                               :code (code "(c/boxpane :orientation :vert"
                                           "            :styles {:fill true}"
                                           "            (c/boxpane (c/label :text \"Name\")"
                                           "                        (c/text-input :prompt \"last\"))"
                                           "            (c/separator :heading \"Separator\")"
                                           "            (c/boxpane (c/label :text \"Adress\")"
                                           "                        (c/text-input :prompt \"street\")))")}
                              {:name "Table-pane container"
                               :description "Describes the format of the :cols parameter of a table-pane."
                               :code (code "(c/table-pane"
                                           " ;; table-pane requires a column-spec"
                                           " ;; :cols is a vector where each element denotes the size of a column"
                                           " ;;   a `-1' is a column with the required with to display its contents"
                                           " ;;   a `[n]' defines a relative column width, where n is a positive integer"
                                           " ;;   a `n' defines an absolute column width"
                                           " ;; for example: [-1 [10] [10] 50] defines a fitting-width colum on the left side,"
                                           " ;; and a 50pixel wide column at the rightmost with two equally wide columns in the middle."
                                           " ;; for now, we need only 2 columns, the first for the label,"
                                           " ;; the second for the text-input"
                                           " :cols [-1 [10]]"
                                           " ;; no a list of row definitions, each row is enclosed in a table-pane-row form"
                                           " (c/table-pane-row (c/label :text \"Name\")"
                                           "                   (c/text-input :prompt \"last\"))"
                                           " (c/table-pane-row (c/separator :heading \"Separator\"))"
                                           " (c/table-pane-row (c/label :text \"Adress\")"
                                           "                   (c/text-input :prompt \"street\")))")}]})


(do (reset! tutorials [])
    (swap! tutorials conj converter)
    (swap! tutorials conj example-form))









