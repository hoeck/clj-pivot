;   Copyright (c) 2009, Erik Soehnel All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

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

