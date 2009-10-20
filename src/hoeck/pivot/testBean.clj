
(hoeck.pivot.genbean/ns-genbean hoeck.pivot.testBean :name :city-code :mobile-phone-number)

(comment
  (binding [*compile-path* ;;"f:/clj/clj-pivot/classes"
            "/home/timmy-turner/clj/clj-pivot/classes"]
    (require 'hoeck.pivot.genbean :reload)
    (require 'hoeck.pivot.testBean :reload)
    (compile 'hoeck.pivot.testBean))
  hoeck.pivot.testBean
  (def tbean (hoeck.pivot.testBean.))
  (def tbean (hoeck.pivot.testBean. {:name 'a :city-code :foo873 :mobile-phone-number 123}))
  (bean tbean)
  (.setMobilePhoneNumber tbean 987))

