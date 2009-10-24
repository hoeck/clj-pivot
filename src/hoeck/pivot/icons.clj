
(ns hoeck.pivot.icons
  (:import (org.apache.pivot.wtk.media Picture Image)
           (java.net URL)))

(defn- symbol->string-ressource-path [s]
  (str (.replaceAll (str s) "\\." "\\/") ".png"))

(defn- get-icon-from-ressource-path [arg]
  (let [rp (if (symbol? arg)
             (symbol->string-ressource-path arg)
             arg)]
    (when-let [url (ClassLoader/getSystemResource rp)]
      (Picture/load url))))

(let [nf (delay (get-icon-from-ressource-path "hoeck/pivot/icons/exclamation-red.png"))]
  (defn not-found-icon []
    (force nf)))

(defn get-icon [arg]
  (cond (or (symbol? arg) (string? arg))
          (or (get-icon-from-ressource-path arg)
              (not-found-icon))
        (isa? arg Image) arg))

