;; tooltips
(ns demo)

;; ---title---
;; Using Tooltips
;; ---description---
;; clj-pivot - tooltips

;; ---title---
;; (0) setup
;; ---description---
;; Require components, listeners and pivot
;; ---code---
(require '[hoeck.pivot :as pivot])
(require '[hoeck.pivot.components :as c])
(require '[hoeck.pivot.listeners :as l])

;; ---title---
;; (1) Standard tooltips
;; ---description---
;; Create and show a push-button with a tooltip text
;; ---code---
(show (c/push-button :data "Hello"
                     :tooltip-text "Press me"))

;; ---title---
;; (2) Standard tooltips #2
;; ---description---
;; Each Component has a :tooltip-text property
;; ---code---
(show (c/boxpane
       (c/label :tooltip-text "Describing the button" "Hello")
       ;; tooltips are limited to a single-row of text
       (c/push-button :data "World" :tooltip-text "Press\nme")))

;; ---title---
;; (3) Customized multiline tooltips
;; ---description---
;; Implement multiline tooltips using the pivot 1.5 Tooltip component
;; ---code---
(defn show-tooltip
  "Show the tooltip from the components [:user :tooltip] property."
  [c]
  (when-let [tt-text (-> c (c/get-property :user) :tooltip)]
    (let [tt (c/tooltip tt-text)
          disp (.getDisplay c)
          loc (.getMouseLocation disp)
          [_ tt-height _] (c/get-property tt :preferred-height)]
      (doto tt
        (.setLocation
         (+ (.x loc) 16)
         (if (< (.getHeight disp) (+ (.y loc) tt-height))
           (- (.y loc) tt-height)
           (.y loc)))
        (.open (.getWindow c))))))

(def tooltip-callback (atom nil))

(defn cancel-tooltip-callback [c]
  (when-let [cb @tooltip-callback] (.cancel cb))
  (reset! tooltip-callback nil))

(defn set-tooltip-callback [c]
  (when-let [cb #(show-tooltip c)]
    (let [scb (org.apache.pivot.wtk.ApplicationContext/scheduleCallback cb 1000)]
      (reset! tooltip-callback scb))))

(defn add-tooltip-listeners
  "Create and add listeners to a component so that tooltips will be displayed
  if the :tooltip userdata property is non-nil.
  :tooltip may be a (multiline-) string or a component."
  [c]
  (l/add-listener
   c
   (l/component-mouse-listener
    _
    :mouse-move (do (cancel-tooltip-callback c) (set-tooltip-callback c) false)
    :mouse-out (cancel-tooltip-callback c)
    nil)
   (l/component-mouse-button-listener
    _
    :mouse-down (do (cancel-tooltip-callback c) false)
    false)))

;; show the label component with the user tooltip property
(show (c/boxpane
       (c/label :user {:tooltip "Describing\nthe\nbutton"
                       :name ::label}
                "Hello")
       ;; tooltips are limited to a single-row of text
       (c/push-button :data "World" :tooltip-text "Press\nme")))

;; register the custom listeners so that the customized tooltip shows up on the label
(add-tooltip-listeners (c/find-component pivot/display ::label))

