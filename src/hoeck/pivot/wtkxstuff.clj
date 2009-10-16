

(defn emit-element [e]
  (if (instance? String e)
    (println e)
    (do
      (print (str "<" (name (:tag e))))
      (when (:attrs e)
	(doseq [attr (:attrs e)]
	  (print (str " " (name (key attr)) "=\"" (val attr) "\""))))
      (if (:content e)
	(do
	  (println ">")
	  (doseq [c (:content e)]
	    (emit-element c))
	  (println (str "</" (name (:tag e)) ">")))
	(println "/>")))))

(defn emit [x]
  (println "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
  (emit-element x))

(defn make-wtkx
  ([xml-map] (make-wtkx (WTKXSerializer.) xml-map))
  ([serializer xml-map]
     (let [i (java.io.ByteArrayInputStream. (.getBytes (with-out-str (emit xml-map))))]
       (.readObject serializer i))))


;;(pprint (xml/parse "d:/al_java/examples/accordion.xml"))

(def accordion-wtkx
     {:tag :BoxPane, :attrs {:xmlns:wtkx "http://pivot.apache.org/wtkx",
			     :xmlns:content "org.apache.pivot.wtk.content",
			     :xmlns "org.apache.pivot.wtk"},
      :content [{:tag :Border,
		 :attrs nil,
		 :content [{:tag :content,
			    :attrs nil,
			    :content [{:tag :BoxPane,
				       :attrs {:orientation "vertical", :styles "{padding:{top:2, left:4, bottom:4, right:4}, spacing:6}"},
				       :content [{:tag :Label,
						  :attrs {:text "Accordion", :styles "{fontBold:true}"},
						  :content nil}
						 {:tag :Accordion,
						  :attrs {:selectedIndex "1", :preferredWidth "110", :preferredHeight "180"},
						  :content [{:tag :panels,
							     :attrs nil,
							     :content [{:tag :Label,
									:attrs {:Accordion.label "One",
										:text "Un",
										:styles "{horizontalAlignment:'center', verticalAlignment:'center', fontItalic:true}"},
									:content nil}
								       {:tag :Label,
									:attrs {:Accordion.label "Two",
										:text "Deux",
										:styles "{horizontalAlignment:'center', verticalAlignment:'center', fontItalic:true}"},
									:content nil}
								       {:tag :Label,
									:attrs {:Accordion.label "Three",
										:text "Trois",
										:styles "{horizontalAlignment:'center', verticalAlignment:'center', fontItalic:true}"},
									:content nil}]}]}]}]}]}]})

;(.readObject (WTKXSerializer.) (URL. "file:///home/timmy-turner/clj/clj-pivot/examples/demo.wtkx"))

;(show application-window)
;(.setContent (@application-state :window) (make-wtkx accordion-wtkx))

