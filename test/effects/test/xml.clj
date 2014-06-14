(ns effects.test.xml
  (:refer-clojure :exclude [for extend])
  (:require [effects :refer :all]
            [effects.free :refer :all])
  (:import [effects.free Free]))

(defprotocol XML
  (xml [v]))

(deftype XMLStr [s]
  Applicative
  (fapply* [_ args]
    (XMLStr. (apply s (map #(.s %) args)))))

(defn make-xml [free-tag]
  (.s (evaluate free-tag ->XMLStr xml)))

(deftype Tag [name attr contents]
  XML
  (xml [t]
    ;; ignoring the attributes for now
    ;; easy to add later
    (XMLStr. (str "<" (.name t) ">\n"
                  (make-xml (fmap contents #(apply str %)))
                  "\n</" (.name t) ">\n"))))

(defn tag [name]
  (fn [attr & contents]
    (let [contents (map (fn [v]
                          (if (= (type v) Free)
                            v
                            (pure v)))
                        contents)]
      (free (Tag. name attr (fapply* (pure list) contents))))))

(def html (tag "html"))
(def head (tag "head"))
(def title (tag "title"))
(def body (tag "body"))
(def p (tag "p"))

(def page (html {}
                (head {}
                      (title {} "This Is The Title"))
                (body {}
                      (p {} "first paragraph")
                      (p {} "second paragraph")
                      (p {} "third paragraph"))))

(println)
(println "---------------")
(print (make-xml page))
