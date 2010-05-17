
;;; clj-pivot - celsius converter tutorial


;;  (0)
;; this little tutorial will, step by step, guide you to implement the swing
;; example from clojure.org (http://clojure.org/jvm_hosted) using
;; apache pivot and clj-pivot. 
;; to head start into clj-pivot, download/unzip and put the following files
;; on your classpath

;; apache pivot
http://apache.mirroring.de/pivot/binaries/apache-pivot-1.4.zip
;; clojure:
http://build.clojure.org/job/clojure/lastSuccessfulBuild/artifact/clojure.jar
;; clojure-contrib
http://build.clojure.org/job/clojure-contrib/lastSuccessfulBuild/artifact/target/clojure-contrib-1.2.0-SNAPSHOT.jar
;; and finally clj-pivot
http://github.com/downloads/hoeck/clj-pivot/clj-pivot.jar

;; note: clj-pivot requires clojure-1.2-snapshot
;; (the latest clojure development version)

;; to setup a standalone clojure repl, call:
;; java -cp <path-to-downloaded-jars>/* clojure.main


;;  (1) setup & test
;; we're going to develop interactively, so we set up a pivot frame
;; and remember pivots root component, the display, so we can change
;; its contents from the repl without restarting the jvm.

;; to follow this tutorial, read the comments and evaluate the expressions
;; by invoking them from a IDE or copying to the clojure-repl

;; first, we have to require the pivot toplevel namespace with:
(require '[hoeck.pivot :as pivot])
;; to use any components, we have to use:
(require '[hoeck.pivot.components :as c])
;; thats it

;; pivot/start takes a function which is called at startup time
;; and may be ommitted
(pivot/start) ;; this shows an empty pivot frame

;; the pivot-do macro executes the given code in the pivot thread,
;; returns its result as a promise
;; if an exception occurs in the body, it will be thrown in the
;; calling thread, e.g. the REPL
;; now were testing the created frame
(pivot/pivot-do (pivot/show @pivot/*display* (cm/push-button :data "convert")))
 
;; pivot/show takes a display and a component and shows it on the display
;; push-button is a function to construct standard buttons
;; like all component functions, it takes arguments as :key value pairs
;; see (doc push-button) to see all available arguments

;; to make the above code shorter, pivot contains macro, similar to ->
(pivot/disp-> (pivot/show (cm/push-button :data "CONVERT")))

;; to have a basis for this howto, we make it even shorter
(defmacro with-pivot-show [& body]
  (pivot/disp-> (pivot/show (do ~@body))))

;;  (2) the components
;; now we take a look at the components we need for our little converter
(with-pivot-show
  (let [ ;; we need:
        ;; a textinput
        celsius-input (cm/text-input :text "23")
        ;; a button
        convert-button (cm/push-button :data "Convert")
        ;; two labels
        celsius-label (cm/label "Celsius")
        fahrenheit-label (cm/label "73.4 Fahrenheit")]
    ;; to be able to see them all at once on our display,
    ;; we wrap them in a container (a boxpane in this case)
    ;; and stack them vertically
    (cm/boxpane
     :orientation :vert
     celsius-input
     convert-button
     celsius-label
     fahrenheit-label)))

;;  (3) first functionality
;; we first need a function to convert a temperature
(defn convert [input-str]
  (let [c (try (Double/parseDouble input-str) (catch NumberFormatException e nil))]
    (str (if c (+ 32 (* 1.8 c)) "-") " Fahrenheit")))
;; and test it:
(convert "23")
;; returns "73.4 Fahrenheit"
(convert "foo")
;; returns "- "Fahrenheit"

;; next, we wire up the convert function to the convert button:
;; the push-button has an :action arg, which is a function without
;; arguments, called when someone clicks the button
(with-pivot-show
  (let [celsius-input (cm/text-input :text "23")
        celsius-label (cm/label "Celsius")
        fahrenheit-label (cm/label "73.4 Fahrenheit")
        convert-button (cm/push-button :data "Convert"
                                       :action #(cm/set-property
                                                 ;; will set the :text -property
                                                 ;; of fahrenheit-label
                                                 fahrenheit-label :text
                                                 (convert (cm/get-property
                                                           celsius-input
                                                           :text))))]
    (cm/boxpane
     :orientation :vert
     celsius-input
     convert-button
     celsius-label
     fahrenheit-label)))


;;  (3) finishing the layout
;; so the application works as excpected, but we have to change the layout
;; to out requirements

(with-pivot-show
  (let [celsius-input (cm/text-input :text "23")
        celsius-label (cm/label "Celsius")
        fahrenheit-label (cm/label "73.4 Fahrenheit")
        convert-button (cm/push-button :data "Convert"
                                     :action #(cm/set-property
                                               fahrenheit-label :text
                                               (convert (-> celsius-input
                                                            (cm/get-property :text)))))]
    ;; we use a table-pane
    ;; this is the most versatile container to achieve various kinds
    ;; of tabular layouts
    (cm/table-pane
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
     (cm/table-pane-row celsius-input celsius-label)
     (cm/table-pane-row convert-button fahrenheit-label))))


;;  (4) cleaning up the code
;; we have a working example, but I'd like to clean this a little
;; bit up and separate the layout from the functionality

;; first we're setting up the components, but this time, we will name them
;; using the :user-name property of each component
;; this allows us to find them later (using (find-component display ::the-name))
;; without holding component-references in lets or global vars
(with-pivot-show
  (cm/table-pane
   :cols [[1] [1]]
   :styles {:vertical-spacing 2
            :horizontal-spacing 2
            :padding [2 2 2 2]}
   (cm/table-pane-row (cm/text-input :text "23"
                                     :user-name ::celsius-input)
                      (cm/label "Celsius"))
   (cm/table-pane-row (cm/push-button :data "Convert"
                                      :user-name ::convert-button)
                      (cm/label :user-name ::fahrenheit-label
                                "73.4 Fahrenheit"))))

;; separate from the layout, we will define the convert functionality
(defn convert-action []
  (let [ti (cm/find-component @*display* ::celsius-input)
        l (cm/find-component @*display* ::fahrenheit-label)]
    (cm/set-property l :text (convert (cm/get-property ti :text)))))

;; now we set the action
(pivot/pivot-do (-> @*display*
                    (cm/find-component ::convert-button)
                    (cm/set-property :action convert-action)))

;;  (5) removing the button
;; ideally, we want the celsius field to be updated whenever the user
;; changes the temperature
(with-pivot-show
  (cm/table-pane
   :cols [[1] [1]]
   :styles {:vertical-spacing 2
            :horizontal-spacing 2
            :padding [2 2 2 2]}
   (cm/table-pane-row (cm/text-input :prompt "Celsius"
                                     :user-name ::celsius-input)
                      (cm/label :user-name ::fahrenheit-label
                                "73.4 Fahrenheit"))))

;; now we add a change-listener to the text-input, to do so, we need
;; the hoeck.pivot.listeners namespace
(require '[hoeck.pivot.listeners :as l])

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
  (l/add-listener (cm/find-component @*display* ::celsius-input)
                  listener))

;; enjoy converting temperatures!



