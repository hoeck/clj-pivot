;   Copyright (c) 2010, Erik Soehnel All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; provide rich components for entering (typed) form data

(ns hoeck.pivot.forms
  (:use hoeck.pivot.components
        hoeck.pivot.listeners
        hoeck.pivot.datastructures)
  (:import (org.apache.pivot.wtk TextInput ListButton BoxPane StackPane
				 CalendarButton)
           (org.apache.pivot.util CalendarDate Vote)
           (java.util Calendar Date)
           (java.text DateFormat DecimalFormat ParseException)
           (java.sql Timestamp)))

(def invalid-background-color [180 0 0 60])


;; data binding

(defn set-component-map
  "given a component, .load the given hash-map."
  [component m]
  (when (and component m)
    (.load component (if m (make-dictionary m) m))))

(defn get-component-map
  "given a component, return a hash-map of its bound values."
  [component]
  (when component
    (let [d (make-dictionary {})]
      (.store component d)
      (dictionary->hashmap d))))


;; typed text-inputs

(defn bigdec-validator [s]
  (or (empty? (.trim s))
      (re-matches #"[0-9]*,?[0-9]+" (.trim s))))

(defn read-bigdec
  "Interpret s as a BigDecimal and return it or 0.0M otherwise"
  ([s] (read-bigdec s (BigDecimal. 0)))
  ([s default]
     (try (.parse (doto (DecimalFormat/getInstance)
                    (.setParseBigDecimal true))
                  (str s))
          (catch ParseException pe default))))

(defn bigdec-input  "A text input which expects a decimal number as input.
  For .load and .store, returns BigDecimal instead of String.
  args: all args to text-input, except :self and:
    :format .. the format string to display the decimal, defaults to \"%.4f\"
  :text-key is required."
  [& args]
  (let [args (apply hash-map args)] 
    (text-input (merge {:self (proxy [TextInput] []
                                (load [m] (when (.containsKey m (get-property this :text-key))
                                            (let [v (.get m (get-property this :text-key))]
                                              (set-property this :text
                                                            (format (:format args "%.4f")
                                                                    (cond (instance? BigDecimal v) v
                                                                          (nil? v) (BigDecimal. 0)
                                                                          (number? v) (BigDecimal. v)
                                                                          :else (throw (IllegalArgumentException. (str "expect some kind of number to bigdec-input, not" v)))))))))
                                (store [m] (.put m (get-property this :text-key) (read-bigdec (get-property this :text)))))
                        :validator bigdec-validator
                        :styles (merge (:styles args) {:invalid-background-color invalid-background-color})}
                        (dissoc args :format :styles)))))

;; a nil aware text-input

(defn safe-text-input
  "Like textinput, but handle a load of a nil value as an empty string."
  [& args]
  (apply text-input 
         :self (proxy [TextInput] []
                 (load [m] (let [k (.getTextKey this)]
                             (when (.containsKey m k)
                               (if (.get m k)
                                 (proxy-super load m)
                                 (proxy-super load 
                                              (make-dictionary {k ""})))))))
         args))

(defn safe-list-button
  "Listbutton wich does not blow up on nil data loads."
  [& args]
  (apply list-button
         :self (proxy [ListButton] []
                 (load [m] (let [k (.getSelectedItemKey this)]
                             (when (.containsKey m k)
                               (if (.get m k)
                                 (proxy-super load m)
                                 (proxy-super load 
                                              (make-dictionary {k ""})))))))
         args))


(defn make-dictionary-button-list-data [r display-key value-key]
  (make-list (map #(make-list-item {:text (get % display-key)
                                    :value (get % value-key)})
                  r)))

(defn dictionary-button
  "A list-button component to choose a single tuple from a relation of tuples."
  [relation display-key value-key & listbutton-args]
  (let [d-data (make-dictionary-button-list-data relation display-key value-key)
        find-item (fn [v] (first (filter #(= (:value %) v) d-data)))]
    (apply list-button
           :self (proxy [ListButton] []
                   (load [m] (let [k (.getSelectedItemKey this)]
                               (when (and m (.containsKey m k))
                                 (let [li (find-item (.get m k))]
                                   (if (nil? li)
                                     (.setSelectedIndex this -1) ;; clear
                                     (set-property this :selected-item li))))))
                   (store [m] (.put m (.getSelectedItemKey this)
                                    (when-let [li (get-property this :selected-item)]
                                      (get li :value)))))
           :list-data d-data
           listbutton-args)))

(comment
  (dictionary-button #{{:id 1 :name "A"} {:id 2 :name "B"}} :name :id
                     :selected-item-key :a)
    
  )

;; timestamp control

(defn set-timestamp-time
  "Set hours, minutes of a Date or Timestamp using a string of \"[h]h:mm\".
  If s is not of this form, don't set anything.
  Returns the Date or Timestamp."
  [ts s]
  (let [[_ h m] (first (re-seq #"([0-9]?[0-9]):([0-9][0-9])" s))]
    (if (and h m)
      (-> (doto (Calendar/getInstance)
            (.setTime ts)
            (.set Calendar/HOUR (Integer/valueOf h))
            (.set Calendar/MINUTE (Integer/valueOf m)))
          .getTimeInMillis
          Timestamp.)
      ts)))

(defn timestamp->timestr
  "Return \"hh:mm\" from a Date."
  [ts]
  (-> (DateFormat/getTimeInstance DateFormat/SHORT) (.format ts)))

(defn current-timestamp
  "Return a timestamp of the current date/time, optionally"
  []
  (-> (Calendar/getInstance)
      .getTimeInMillis
      Timestamp.))

(defn timestamp->caldate [timestamp]
  (CalendarDate. (doto (Calendar/getInstance) (.setTime timestamp))))

(defn caldate->timestamp [calendar-date]
  (Timestamp. (.getTimeInMillis (.toCalendar calendar-date))))

(defn time-validator [s] (re-matches #"[0-9]{1,2}+:[0-9]{2}" (.trim s)))

(defn date->caldate
  "For some descendant of java.util.Date."
  [date]
  (CalendarDate. (doto (Calendar/getInstance) (.setTime date))))

(defn caldate->sqldate [calendar-date]
  (java.sql.Date. (.getTimeInMillis (.toCalendar calendar-date))))

(defn timestamp-input
  "Returns a combo control, consisting of a Calendarbutton and a
  text-input for hh:mm.
  .load and .store deals with java.sql.timestamps.
  args: :timestamp-key .. the key (string or keyword) for data-binding, immutable.
        :text-input .. a textinput to use for time editing (optional)
        :calendar-button .. a calendarbutton to use for date edtiting (optional)"
  [& args]
  (let [args (apply hash-map args)
        ts-key (:timestamp-key args)
        cal (or (:calendar-button args) (calendar-button))
        clk (text-input :self (or (:text-input args) (text-input))
                        :validator time-validator
                        :text-size 6
                        :max-length 10)
        comp (boxpane :self (proxy [BoxPane] []
                              (load [m] (when (.containsKey m (str ts-key))
                                          (let [ts (or (.get m (str ts-key)) (current-timestamp))]
                                            (set-property cal :selected-date (timestamp->caldate ts))
                                            (set-property clk :text (timestamp->timestr ts)))))
                              (store [m] (let [ts (-> (caldate->timestamp (get-property cal :selected-date))
                                                      (set-timestamp-time (get-property clk :text)))]
                                           (.put m (str ts-key) ts))))
                      cal clk)]
    comp))

(defn date-button
  "Returns a pivot CalendarButton which loads and stores a java.sql.Date.
  args: :date-key .. the key (string or keyword) for data--binding, immutable.
        :calendar-button .. optionally a specific calendarbutton to use."
  [& args]
  (let [args (apply hash-map args)
	dkey (str (:date-key args))
	cal (or (:calendar-button args)
                (apply calendar-button (apply concat (dissoc args :date-key :calendar-button))))
	di (stackpane :self (proxy [StackPane] []
			      (load [m] (when (and m (.containsKey m dkey))
                                          (if-let [d (.get m dkey)]
                                            (set-property cal :selected-date (date->caldate d))
                                            (set-property cal :data nil))))
			      (store [m] (let [d (get-property cal :selected-date)]
					   (.put m dkey (caldate->sqldate d)))))
		      cal)]
    di))

;; masks empty forms (load with null), unmasks non empty forms (load with non-nil)

(defn mask-pane
  "Masks empty forms (.load with nil),
   unmasks non empty forms (.load with non-nil).
  args: :msg .. the text of the label which masks component
        :empty? .. when true, mask-pane is initially in empty state
                   defaults to true
        :background-color .. the color to shadow the component with, a vector of
                             [r g b] in ranges from 0-255, defaults to white."
  [& args]
  (let [[args [c]] (parse-component-args args)
	bg-col (or (:background-color args) [255 255 255])
        mask-component (table-pane :cols [[1]]
                                   :visible (:empty? args true)
                                   (table-pane-row 
                                    :height [1] (label :styles {:horizontal-alignment :center
                                                                :vertical-alignment :center
                                                                :background-color (conj bg-col 200)
                                                                :font-italic true}
                                                       :text (:msg args ""))))]
    (stackpane :self (proxy [StackPane] []
                       (load [m]
                             (def _m m)
                             (set-property mask-component :visible (nil? m))
                             (when-not (nil? m) (.load c m))))
               c
               mask-component)))


;; auto-completing text-input

(defn- listview-popup
  "return a popup-window with the appropriate listeners set.
  NEEDS a patched WindowSkin, because its mouseDown handler tries to move the window up
  when it is closed after a list-view-selection event."
  [ti on-select-f]
  (let [lv (listview :user-name ::listview
                     :selected-item-key ::selected-item
                     :preferred-width (get-property ti :preferred-width))
        ;; popup
        popup (window :user-name ::listview-popup
                      :auxilliary true
                      :location (let [b (.getBounds ti)
                                      p (.getLocation b)
                                      p2 (.mapPointToAncestor
                                          ti (.getWindow ti) 0 0) ;;(.x p) (.y p)
                                      x (.x p2)
                                      y (.y p2)]
                                  [x (+ y (.height b) (.getPreferredHeight lv -1) -1)])
                      (border lv))
        ;; popup related listeners
        cml (container-mouse-listener {cont :container [x y] :int}
              :mouse-down (let [w (.getComponentAt cont x y)]
                            (when-not (and (or (not (identical? popup w))
                                               (not (identical? ti w)))
                                           (.isOpen popup))
                              ;; user clicked out of the bounds of this popup
                              (.close popup))
                            false)
              :mouse-wheel true
              false)
        ;; close popup on textinput/listview changes
        til (text-input-listener _ (when (.isOpen popup) (.close popup)))
        lvl (list-view-selection-listener _ 
              (do (on-select-f ti (::selected-item (get-component-map lv)))
                  (when (.isOpen popup) (.close popup))))
        ;; popup listener
        wsl (window-state-listener {w :window :as args}
              :window-opened (do (add-listener (.getDisplay w) cml)
                                 (add-listener ti til)
                                 (add-listener lv lvl))
              :window-closed (do (remove-listener (:display args) cml)
                                 (remove-listener ti til)
                                 (remove-listener lv lvl)
                                 (.requestFocus ti))
              :preview-window-open Vote/APPROVE
              :preview-window-close Vote/APPROVE)]
    (add-listener popup wsl)
    popup))

(defn auto-text-input
  "Returns a text-input wich opens a listview below if a character is typed
  and the :on-complete-f function (called with the current text) returns a 
  list of strings, which are displayed in the listview.
  :on-select-f is called with textinput, item-string when the user picks an
  item from the listview."
  [& ti-args]
  (let [argm (apply hash-map ti-args)
        ti (->> (dissoc argm :on-complete-f :on-select-f)
                (apply concat)
                (apply text-input))
        cf (:on-complete-f argm (constantly nil))
        sf (:on-select-f argm (constantly nil))
        pp (delay (listview-popup ti sf))
        lv (delay (find-component (force pp) ::listview))]
    (add-listener
     ti (text-input-character-listener _
          (let [t (get-property ti :text)
                comp-list (cf t)]
            (when-not (empty? comp-list);; a clojure list
              ;; open a popup below ti and show a listview of possible inputs
              (set-property (force lv) :data (make-list comp-list))
              (.open (force pp) (.getWindow ti))))))
    ti))

(comment
  ;; example
  (auto-text-input
   :user-name ::auto
   :on-complete-f #(when (< 3 (count %)) ["a" "b" "c"])
   :on-select-f (fn [ti v] (set-property ti :text v))))


