
;; Generate JavaBeans from clojure: http://en.wikipedia.org/wiki/JavaBean

(ns hoeck.pivot.genbean)

(defn struct-keys [s]
  (keys (struct s)))

(defn lisp-to-camelcase [first-letter-uppercase? symbol-or-keyword]
  (let [sn (.split (name symbol-or-keyword) "-")
        upcase-first #(str (.toUpperCase (.substring % 0 1)) (.substring % 1))]
    (apply str (cons (if first-letter-uppercase? 
                       (upcase-first (first sn))
                       (first sn))
                     (map upcase-first (next sn))))))

(defn genbean-stuff [keys]
  (let [k keys
        names (map #(lisp-to-camelcase true %) k)
        getters (map #(str "get" %) names)
        setters (map #(str "set" %) names)]
    [getters setters]))

(defmacro genbean-getter [struct-state-var-name getter-name key]
  `(defn ~(symbol (str "-" getter-name)) [this#]
     (get @(. this# ~struct-state-var-name) ~key)))

(defmacro genbean-setter [struct-state-var-name setter-name key]
  `(defn ~(symbol (str "-" setter-name)) [this# o#]
     (swap! (. this# ~struct-state-var-name)
            assoc ~key o#)
     nil))

(defn make-getter-sig [name]
  `[~name [] Object])

(defn make-setter-sig [name]
  `[~name [Object] Void])

(defmacro genbean-map-atom
  "Generate a map, atom based mutable java bean using genclass."
  [bean-name state-name key-list]
  (let [[getter-names setter-names] (genbean-stuff key-list)
        method-signatures (vec (concat (map make-getter-sig getter-names)
                                       (map make-setter-sig setter-names)))
        ctors `{[clojure.lang.IPersistentMap] []
		[] []}
        init-name 'beaninit]
    `(do (gen-class :name ~bean-name
		    :methods ~method-signatures
		    :state ~state-name
		    :init ~init-name
		    :constructors ~ctors)
	 (defn ~beaninit
	   ([] [[] (atom {})])
	   ([initial-map#] [[] (atom initial-map#)]))
	 ~@(map (fn [name key] `(genbean-getter ~state-name ~name ~key)) getter-names key-list)
	 ~@(map (fn [name key] `(genbean-setter ~state-name ~name ~key)) setter-names key-list))))


(defmacro ns-genbean
  "Generate a mutable bean implementation with the given keys as fields using a clojure
  map and an atom to allow a threadsafe mutation.
  The generated bean uses effectively untyped getters and setters (Object getXXX() and void setXXX(Obect o)).
  Given keywords/symbols a java-fied: `:key-word-with-dashes' -> getKeyWordWithDashes, setKeyWordWithDashes."
  [qualified-bean-name & keywords]
  `(do (ns ~qualified-bean-name)
       (genbean-map-atom ~qualified-bean-name beandata# ~keywords)))

(comment ;; a simple mutable bean in clojure using genclass structmap and atom
  (ns foo.bar.CljBean
    (:gen-class :name foo.bar.CljBean
                :methods [[getName [] Object]
                          [setName [Object] Void]
                          [getCityCode [] Object]
                          [setCityCode [Object] Void]]
                :state beandata
                :constructors {[clojure.lang.IPersistentMap] []}
                :init beaninit))

  (defn beaninit [initial-struct-map]
    [[] (atom initial-struct-map)])

  (defn -getName [this]
    (get @(. this beandata) :name))

  (defn -setName [this o]
    (swap! (. this beandata) assoc :name o)
    nil)

  (defn -getCityCode [this]
    (get @(. this beandata) :city-code))

  (defn -setCityCode [this o]
    (swap! (. this beandata) assoc :city-code o)
    nil)

  -> (hoeck.pivot.genbean/ns-genbean foo.bar.CljBean :name :city-code)


  )


