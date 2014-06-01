(ns effects.test.test
  (:refer-clojure :exclude [for extend])
  (:require [effects :refer :all]
            [effects.maybe :refer :all]
            [effects.state :refer :all]
            [effects.free :refer :all]
            [effects.vector :refer :all]
            [effects.reader :refer :all])
  (:import [effects.free Free Pure FreeT FreeA]))

(deftype Tag [name attr contents]
  Object
  (toString [_]
    (pr-str name attr contents)))

(defn tag [name]
  (fn [attr contents]
    (Tag. name attr contents)))

(def html (tag "html"))
(def body (tag "body"))
(def p (tag "p"))

(def page (html {}
                (body {}
                      (p {} "first paragraph"))))

(defprotocol XML
  (xml [v]))

(extend-type Tag
  XML
  (xml [t]
    (let [contents (xml (.contents t))]
      (str "<" (.name t) ">\n" contents "\n</" (.name t) ">"))))

(extend-type java.lang.String
  XML
  (xml [ev] ev))

(println (xml page))
(println)

(deftype Tag [name attr contents next]
  Object
  (toString [_]
    (pr-str name attr contents next)))

(defn tag [name]
  (fn [attr contents next]
    (Tag. name attr contents next)))

(def html (tag "html"))
(def head (tag "head"))
(def title (tag "title"))
(def body (tag "body"))
(def p (tag "p"))

(def page (html {}
                (head {}
                      (title {} "This Is The Title" nil)
                      (body {}
                            (p {} "first paragraph"
                               (p {} "second paragraph" nil))
                            nil))
                nil))

(extend-type Tag
  XML
  (xml [t]
    (let [contents (when (.contents t)
                     (xml (.contents t)))
          next (when (.next t)
                 (str \newline (xml (.next t))))]
      (str "<" (.name t) ">\n" contents "\n</" (.name t) ">" next))))

(println (xml page))
(println)

(defn tag [name]
  (fn [attr & contents]
    (Tag. name attr contents nil)))

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
                      (p {} "second paragraph"))))

;; Errors out
#_(println (xml page))
#_(println)

(extend-type Tag
  EndoFunctor
  (fmap [t f]
    (Tag. (.name t) (.attr t) (.contents t) (f (.next t)))))

;; Explain the Free Monad

(defn tag [name]
  (fn [attr & contents]
    (let [contents (reduce (fn [ev next]
                             (flat-map ev (fn [_] next)))
                           contents)]
      (Free. (fmap (Tag. name attr contents nil)
                   (fn [x] (Pure. x nil)))
             nil))))

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

(extend-type Tag
  XML
  (xml [t]
    (let [contents (xml (.contents t))
          next (xml (.next t))]
      (str "<" (.name t) ">\n" contents "\n</" (.name t) ">"
           (if (= next "")
             ""
             (str \newline next))))))

(extend-type Pure
  XML
  (xml [ev]
    (str (.v ev))))

(extend-type Free
  XML
  (xml [ev]
    (xml (.v ev))))

(println (xml page))
(println)

(defprotocol XML
  (xml [v]))

(deftype Tag [name attr contents next]
  Object
  (toString [_]
    (pr-str name attr contents next))

  EndoFunctor
  (fmap [t f]
    (Tag. (.name t) (.attr t) (.contents t) (f (.next t)))))

(defn tag [name]
  (fn [attr & contents]
    (let [contents (reduce (fn [ev next]
                             (flat-map ev (fn [_] next)))
                           contents)
          contents (if (= Free (type contents))
                     contents
                     (Pure. contents nil))]
      (Free. (Tag. name attr contents (Pure. nil nil)) nil))))

(extend-type Tag
  XML
  (xml [t]
    (let [contents (xml (.contents t))
          next (xml (.next t))]
      (str "<" (.name t) ">\n" contents "\n</" (.name t) ">"
           (str \newline next)))))

(extend-type Pure
  XML
  (xml [ev]
    (str (.v ev))))

(extend-type Free
  XML
  (xml [ev]
    (xml (.v ev))))

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

(println (xml page))
(println)

(extend-type Tag
  XML
  (xml [t]
    (for [contents (xml (.contents t))
          next (xml (.next t))]
      (str "<" (.name t) ">\n" contents "\n</" (.name t) ">"
           (str \newline next)))))

(extend-type Pure
  XML
  (xml [ev]
    (fmap (.v ev) str)))

(extend-type FreeT
  XML
  (xml [ev]
    (flat-map (.mv ev) xml)))

(defn multi-tag [name]
  (fn [attr & contents]
    (let [contents (reduce (fn [ev next]
                             (flat-map ev (fn [_] next)))
                           contents)
          contents (if (= FreeT (type contents))
                     contents
                     (Pure. (vector contents) nil))]
      (FreeT. (vector (Tag. name attr contents (Pure. (vector nil) nil)))))))

(def html (multi-tag "html"))
(def head (multi-tag "head"))
(def title (multi-tag "title"))
(def body (multi-tag "body"))
(def p (multi-tag "p"))

(defn alt [& alternatives]
  (FreeT. (vec alternatives)))

(def page (html {}
                (head {}
                      (alt (title {} "This Is The Title")
                           (title {} "Alternate Title")))
                (body {}
                      (p {} "first paragraph")
                      (p {} "second paragraph")
                      (p {} "third paragraph"))))

(prn (xml page))
(println)

(defn template-tag [name]
  (fn [attr & contents]
    (let [contents (reduce (fn [ev next]
                             (flat-map ev (fn [_] next)))
                           contents)
          contents (if (= FreeT (type contents))
                     contents
                     (Pure. (reader contents) nil))]
      (FreeT. (reader (Tag. name attr contents (Pure. (reader nil) nil)))))))

(def html (template-tag "html"))
(def head (template-tag "head"))
(def title (template-tag "title"))
(def body (template-tag "body"))
(def p (template-tag "p"))

(def page (html {}
                (head {}
                      (title {} "This Is The Title"))
                (body {}
                      (p {} "first paragraph")
                      (p {} "second paragraph")
                      (p {} "third paragraph"))))


;; (println ((xml page) :environment))
;; (println)

(defn insert [k]
  (FreeT. (fmap (read-val k) #(Pure. (reader %) nil))))

(def page (html {}
                (head {}
                      (title {} (insert :title)))
                (body {}
                      (p {} "first paragraph")
                      (p {} (insert :second-para))
                      (p {} "third paragraph"))))

(println ((xml page) {:title "This Is The Title"
                      :second-para "second paragraph"}))
(println)

(println ((xml page) {:title "Another Title"
                      :second-para "a better paragraph"}))
(println)

;; ;; *****************
;; ;; Formatted XML

;; (defprotocol XML
;;   (format-xml* [ev wrapper indent]))

;; (defn spaces [n] (apply str (repeat n " ")))

;; (extend-type Pure
;;   XML
;;   (format-xml* [ev wrapper indent]
;;     (wrapper (str (extract ev)))))

;; (extend-type FreeT
;;   XML
;;   (format-xml* [ev _ indent]
;;     (let [v (extract ev)]
;;       (flat-map v #(format-xml* % (partial wrap v) indent)))))

;; #_(extend-type java.lang.String
;;   XML
;;   (format-xml* [s wrapper indent]
;;     (wrapper (str (spaces indent) s))))

;; (extend-type Tag
;;   XML
;;   (format-xml* [t wrapper indent]
;;     (for [contents (format-xml* (.contents t) wrapper (+ indent 3))
;;           next (format-xml* (.next t) wrapper indent)]
;;       (str (spaces indent) "<" (.name t) ">\n" contents "\n"
;;            (spaces indent) "</" (.name t) ">"
;;            (if (= next "")
;;              ""
;;              (str \newline next))))))

;; (defn formatted-page-maker [page]
;;   (format-xml* page reader 0))

;; (def format-xml (formatted-page-maker page))

;; (println (format-xml {:title "This Is The Title"
;;                       :second-para "second paragraph"}))
;; (println)

;; (deftype P [attr contents next]
;;   EndoFunctor
;;   (fmap [_ f]
;;     (P. attr contents (f next)))

;;   XML
;;   (format-xml* [t wrapper indent]
;;     (for [contents (format-xml* (.contents t) wrapper 0)
;;           next (format-xml* (.next t) wrapper indent)]
;;       (str (spaces indent) "<p>" contents  "</p>"
;;            (if (= next "")
;;              ""
;;              (str \newline next))))))

;; (defn p [attr & contents]
;;   (let [contents (reduce (fn [ev next]
;;                            (flat-map ev (fn [_] next)))
;;                          contents)]
;;     (FreeT. (reader (fmap (P. attr contents nil)
;;                           (fn [x] (Pure. x nil)))))))

;; (def page (html {}
;;                 (head {}
;;                       (title {} (insert :title)))
;;                 (body {}
;;                       (p {} "first paragraph")
;;                       (p {} (insert :second-para))
;;                       (p {} "third paragraph"))))

;; (def format-xml (formatted-page-maker page))

;; (println (format-xml {:title "This Is The Title"
;;                       :second-para "second paragraph"}))
;; (println)
