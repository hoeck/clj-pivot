
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
  `(defn ~(symbol getter-name) [this#]
     (get @(. this# ~struct-state-var-name) ~key)))

(defmacro genbean-setter [struct-state-var-name setter-name key]
  `(defn ~(symbol setter-name) [this# o#]
     (swap! (. this# ~struct-state-var-name)
            assoc ~key o#)
     nil))

(defn make-getter-sig [name]
  `[~name [] Object])

(defn make-setter-sig [name]
  `[~name [Object] Void])

(defmacro genbean-map-atom
  "Generate a map, atom based mutable java bean using genclass. Use the lowercased classname as a prefix for
  the method implementations to allow more than one bean safely defined in one Namespace."
  [bean-name & key-list]
  (let [[getter-names setter-names] (genbean-stuff key-list)
        method-signatures (vec (concat (map make-getter-sig getter-names)
                                       (map make-setter-sig setter-names)))
        ctors `{[clojure.lang.IPersistentMap] []
		[] []}
        init-name 'beaninit
	state-name 'beanstate
	prefix (str (last (.split (.toLowerCase (str bean-name)) "\\.")) "-")]
    `(do (gen-class :name ~bean-name
		    :methods ~method-signatures
		    :state ~state-name
		    :init ~init-name
		    :prefix ~prefix
		    :constructors ~ctors)
	 (defn ~(symbol (str prefix init-name))
	   ([] [[] (atom {})])
	   ([initial-map#] [[] (atom initial-map#)]))
	 ~@(map (fn [name key] `(genbean-getter ~state-name ~(str prefix name) ~key)) getter-names key-list)
	 ~@(map (fn [name key] `(genbean-setter ~state-name ~(str prefix name) ~key)) setter-names key-list))))

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

  -> (hoeck.pivot.genbean/ns-genbean foo.bar.CljBean :name :city-code) or

  (ns 'foo.bar.Baz
    (:use hoeck.pivot.genbean))

  ... code ...
  
  (genbean-map-atom foo.bar.MyBean :name :city-code)


  )


