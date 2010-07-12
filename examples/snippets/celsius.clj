
;; clj-pivot celsius converter howto
(ns demo)

;; ---title---
;; Celsius Converter
;; ---description---
;; clj-pivot - celsius converter tutorial snippets

;; ---title---
;; (1) setup and test
;; ---description---
;; From clj-pivot, we need components and listeners
;; ---code---
(require '[hoeck.pivot :as pivot])
(require '[hoeck.pivot.components :as c])
(require '[hoeck.pivot.listeners :as l])

(defmacro show [& body]
  `(pivot-> (pivot/show (do ~@body))))

;; ---title---
;; (2) clj-pivot hello world
;; ---description---
;; create and show a push-button
;; ---code---
(show (c/push-button :data "Hello"))

;; ---title---
;; (3) the components
;; ---description---
;; now we take a look at the components we need for our little converter
;; ---code---
(show
 (let [ ;; we need:
       ;; a textinput
       celsius-input (c/text-input :text "23")
       ;; a button
       convert-button (c/push-button :data "Convert")
       ;; two labels
       celsius-label (c/label "Celsius")
       fahrenheit-label (c/label "73.4 Fahrenheit")]
   ;; to be able to see them all at once on our display,
   ;; we wrap them in a container (a boxpane in this case)
   ;; and stack them vertically
   (c/boxpane
    :orientation :vert
    celsius-input
    convert-button
    celsius-label
    fahrenheit-label)))

;; ---title---
;; (4) basic functionality
;; ---description---
;; Define a function to convert fahrenheits to celsius.
;; Test the function on the repl.
;; ---code---
(defn convert [input-str]
  (let [c (try (Double/parseDouble input-str) (catch NumberFormatException e nil))]
    (str (if c (+ 32 (* 1.8 c)) "-") " Fahrenheit")))
;; and test it:
(convert "23")
;; returns "73.4 Fahrenheit"
(convert "foo")
;; returns "- "Fahrenheit"


;; ---title---
;; (5) button-action
;; ---description---
;; Wire up the convert function to the convert button.
;; The push-button has an :action arg, which is a function without
;; arguments, called when someone clicks the button.
;; ---code---
(show
 (let [celsius-input (c/text-input :text "23")
       celsius-label (c/label "Celsius")
       fahrenheit-label (c/label "73.4 Fahrenheit")
       convert-button (c/push-button :data "Convert"
                                     :action #(c/set-property
                                               ;; will set the :text -property
                                               ;; of fahrenheit-label
                                               fahrenheit-label :text
                                               (convert (c/get-property
                                                         celsius-input
                                                         :text))))]
   (c/boxpane
    :orientation :vert
    celsius-input
    convert-button
    celsius-label
    fahrenheit-label)))


;; ---title---
;; (6) finishing the layout
;; ---description---
;; so the application works as excpected, but we have to change the layout
;; to out requirements
;; ---code---
(show
 (let [celsius-input (c/text-input :text "23")
       celsius-label (c/label "Celsius")
       fahrenheit-label (c/label "73.4 Fahrenheit")
       convert-button (c/push-button :data "Convert"
                                     :action #(c/set-property
                                               fahrenheit-label :text
                                               (convert (-> celsius-input
                                                            (c/get-property :text)))))]
   ;; we use a table-pane
   ;; this is the most versatile container to achieve various kinds
   ;; of tabular layouts
   (c/table-pane
    ;; we have 2 columns, each have a relative width of 1,
    ;; each col will get 50% of the available space
    :cols [[1] [1]]
    ;; now we style the table pane to get some custom spacings
    ;; to see all available styles for eg. a table-pane, type
    ;; (pprint-styles table-pane) the the repl
    :styles {:vertical-spacing 2
             :horizontal-spacing 2
             :padding [2 2 2 2]}
    ;; 2 rows of components
    (c/table-pane-row celsius-input celsius-label)
    (c/table-pane-row convert-button fahrenheit-label))))


;; ---title---
;; (7) cleaning up
;; ---description---
;; Clean up the working example code and refactor it into smaller functions.
;; Create named components with the `:user-name' property and refer to them
;; using `c/find-component'
;; ---code---
(show
 (c/table-pane
  :cols [[1] [1]]
  :styles {:vertical-spacing 2
           :horizontal-spacing 2
           :padding [2 2 2 2]}
  (c/table-pane-row (c/text-input :text "23"
                                  :user-name ::celsius-input)
                    (c/label "Celsius"))
  (c/table-pane-row (c/push-button :data "Convert"
                                   :user-name ::convert-button)
                    (c/label :user-name ::fahrenheit-label
                             "73.4 Fahrenheit"))))

;; separate from the layout, we will define the convert functionality
(defn convert-action []
  (let [r pivot/display
        ti (c/find-component r ::celsius-input)
        l (c/find-component r ::fahrenheit-label)]
    (c/set-property l :text (convert (c/get-property ti :text)))))

;; now we set the action
(pivot/disp-> 
 (c/find-component ::convert-button)
 (c/set-property :action convert-action))

;; ---title---
;; (8) change listeners
;; ---description---
;; Use a listener to update the celsius field whenever the user
;; changes the temperature
;; ---code---
(show
 (c/table-pane
  :cols [[1] [1]]
  :styles {:vertical-spacing 2
           :horizontal-spacing 2
           :padding [2 2 2 2]}
  (c/table-pane-row (c/text-input :prompt "Celsius"
                                  :user-name ::celsius-input)
                    (c/label :user-name ::fahrenheit-label
                             "73.4 Fahrenheit"))))

;; to find out which listener we will need, we're going to look
;; into the pivot apidocs, specifially the page for a TextInput:
;; http://pivot.apache.org/1.4/docs/api/org/apache/pivot/wtk/TextInput.html
;; there, we take a look at the TextInputTextListener:
;; http://pivot.apache.org/1.4/docs/api/org/apache/pivot/wtk/TextInputTextListener.html
;; and spot the textChanged method, now we implement it using clj-pivot:
(let [listener (l/text-input-text-listener
                ;; here we could specify the arguments we need from the
                ;; text-input-text-listener
                ;; we need no args, so we put a underscore there,
                ;; which is idiomatic clojure naming for "dont care"
                _
                ;; this is the body of our listener
                ;; the text-input-text-listener has only one method,
                ;; so we specify a single form here which will be executed
                ;; on all invocations of this listener
                (convert-action))]
  ;; add the listener to the desired component:
  (l/add-listener (c/find-component pivot/display ::celsius-input)
                  listener))




