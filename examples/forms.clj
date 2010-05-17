
;; snippets to be used on the repl or within the example applet
(ns demo)

(defmacro show [& body]
  `(pivot-> (pivot/show (do ~@body))))

;; ---title---
;; forms tutorial
;; ---description---
;; learn how to use a table-pane to lay out forms

;; ---title---
;; setup
;; ---description---
;; set up aliases to the required clj-pivot namespaces
;; ---code---
(require '[hoeck.pivot :as pivot])
(require '[hoeck.pivot.components :as cm])

;; ---title---
;; form components
;; ---description---
;; To create a form similar to the one at: (warning: loads an applet)
;; http://pivot.apache.org/tutorials/forms.html, 
;; separators, labels and textinputs are required.
;; ---code---
(show
 (c/boxpane :orientation :vert
            :styles {:fill true}
            (c/label :text "Label")
            (c/separator :heading "Separator")
            (c/text-input :prompt "text-input")))

;; ---title---
;; boxpane container
;; ---description---
;; Trying to lay out form components with a boxpane container.
;; ---code---
(show
 (c/boxpane :orientation :vert
            :styles {:fill true}
            (c/boxpane (c/label :text "short label")
                       (c/text-input :prompt "last"))
            (c/separator :heading "Separator")
            (c/boxpane (c/label :text "loooooooooooog Label")
                       (c/text-input :prompt "street"))))


;; ---title---
;; table-pane columns
;; ---description---
;; A table-pane will do a better job at laying out forms

;; ---code---
(show
 (c/table-pane
  ;; table-pane requires a column-spec
  ;; :cols is a vector where each element denotes the size of a column
  ;;   a `-1' is a column with the required with to display its contents
  ;;   a `[n]' defines a relative column width, where n is a positive integer
  ;;   a `n' defines an absolute column width
  ;; for example: [-1 [10] [10] 50] defines a fitting-width colum on the left side,
  ;; and a 50pixel wide column at the rightmost with two equally wide columns in the middle.

  ;; for now, we need only 2 columns, the first for the label,
  ;; the second for the text-input
  :cols [-1 [10]]

  ;; no a list of row definitions, each row is enclosed in a table-pane-row form
  (c/table-pane-row (c/label :text "Name")
                    (c/text-input :prompt "last"))
  (c/table-pane-row (c/separator :heading "Separator"))
  (c/table-pane-row (c/label :text "Adress")
                    (c/text-input :prompt "street"))))


;; ---title---
;; table-pane column spanning
;; ---description---
;; Make the separator span over multiple columns.
;; ---code---
(show
 (c/table-pane
  :cols [-1 [10]]

  (c/table-pane-row (c/label :text "Name")
                    (c/text-input :prompt "last"))
  (c/table-pane-row
   ;; to let a component in a row wrap over multiple rows and columns,
   ;; enclose it in a vector and put an [cols, rows] pair at the end.
   ;; [cols, rows] means, let the component wrap over those columns or rows
   ;; a star means don't care, you may also omit the rows and
   ;; only specify [cols]
   [(c/separator :heading "Separator") [2 *]])
  (c/table-pane-row (c/label :text "Adress")
                    (c/text-input :prompt "street"))))

;; ---title---
;; table-pane label alignment
;; ---description---
;; Use label :styles to align the label-text with the textinput-text
;; horizontally
;; ---code---
(show
 (c/table-pane
  :cols [-1 [10]]

  (c/table-pane-row (c/label
                     ;; by using a :centered vertical alignment, the label and
                     ;; the textinput align their text on the same line
                     :styles {:vertical-alignment :center}
                     :text "Name")
                    (c/text-input :prompt "last"))
  (c/table-pane-row
   [(c/separator :heading "Separator") [2 *]])
  (c/table-pane-row (c/label
                     :styles {:vertical-alignment :center}
                     :text "Adress")
                    (c/text-input :prompt "street"))))

;; ---title---
;; the whole form
;; ---description---
;; generating the whole name-addres-contact form
;; ---code---
(show
 (cm/table-pane
  :styles {:horizontal-spacing 3
           :vertical-spacing 3
           :padding [2 2 2 2]}
  :cols [-1 [60] [20] [20]]
  (cm/table-pane-row (label "Name:")
                     (cm/text-input :prompt "last")
                     [(cm/text-input :prompt "first") [2]])
  (cm/table-pane-row [(cm/separator :heading "Adress") [4]])
  (cm/table-pane-row (label "Home:")
                     [(cm/text-input :prompt "street") [3]])
  (cm/table-pane-row (cm/filler)
                     (cm/text-input :prompt "city")
                     (cm/text-input :prompt "state")
                     (cm/text-input :prompt "zip"))
  (cm/table-pane-row :height 0 (cm/filler))
  (cm/table-pane-row (label "Work:")
                     [(cm/text-input :prompt "street") [3]])
  (cm/table-pane-row (cm/filler)
                     (cm/text-input :prompt "city")
                     (cm/text-input :prompt "state")
                     (cm/text-input :prompt "zip"))
  (cm/table-pane-row [(cm/separator :heading "Phone Numbers") [4]])
  (cm/table-pane-row (label "Home:") [(cm/text-input) [2]])
  (cm/table-pane-row (label "Work:") [(cm/text-input) [2]])
  (cm/table-pane-row [(cm/separator :heading "Email Addresses") [4]])
  (cm/table-pane-row (label "Home:") [(cm/text-input) [2]])
  (cm/table-pane-row (label "Work:") [(cm/text-input) [2]])))


