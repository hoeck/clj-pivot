
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
        getters-map (zipmap k (map #(str "get" %) names))
        setters-map (zipmap k (map #(str "set" %) names))]
    [getters-map setters-map]))

(defmacro genbean-setter [struct-state-var-name setter-name key]
  `(defn ~(symbol (str "-" setter-name)) [this# o#]
     (swap! (. this ~struct-state-var-name)
            assoc ~key o#)))

(defmacro genbean-getter [struct-state-var-name getter-name key]
  `(defn ~(symbol (str "-" getter-name)) [this# o#]
     (get @(. this ~struct-state-var-name) ~key)))

(defn make-getter-sig [name]
  `[~name [] Object])

(defn make-setter-sig [name]
  `[~name [Object] Void/TYPE])

(defmacro genbean [bean-name state-name key-list]
  (let [[getter-names setter-names] (genbean-stuff key-list)
        method-signatures (vec (concat (map make-getter-sig getter-names)
                                       (map make-setter-sig setter-names)))
        ctors `{[clojure.lang.IPersistentMap] []}
        init-name 'beaninit
        ns-clause `(ns ~bean-name
                     (:gen-class :name ~bean-name)
                     (:methods ~method-signatures)
                     (:state ~state-name)
                     (:init ~init-name)
                     (:constructors ~ctors))]
    
    ))

(defstruct foos :a-value :name :city-code)
(genbean* foos)
 
(comment ;; a bean in clojure
  (ns foo.bar.CljBean
    (:gen-class :name foo.bar.CljBean
                :methods [[getName [] Object] 
                          [setName [Object] Void/TYPE]
                          [getCityCode [] Object]
                          [setCityCode [Object] Void/TYPE]]
                :state beandata
                :constructors {[clojure.lang.IPersistentMap] []}
                :init beaninit))

  (defn beaninit [initial-struct-map]
    [[] (atom initial-struct-map)])

  (defn -getName [this]
    (get @(. this beandata) :name))

  (defn -setName [this o]
    (swap! (. this beandata) assoc :name o))

  (defn -getCityCode [this]
    (get @(. this beandata) :city-code))

  (defn -setCityCode [this o]
    (swap! (. this beandata) assoc :city-code o))

  )