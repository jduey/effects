
(deftype Tag [tag attr contents]
  Object
  (toString [_]
    (pr-str tag attr contents))

  e/EndoFunctor
  (fmap [_ f]
    (Tag. tag attr (f contents)))

  e/Comonad
  (extract [_] tag))

(deftype P [attr contents next]
  Object
  (toString [_]
    (pr-str attr contents next))

  e/EndoFunctor
  (fmap [ev f]
    (P. attr contents (f next)))

  e/Comonad
  (extract [_] contents))

(deftype Title [attr contents next]
  Object
  (toString [_]
    (pr-str attr contents next))

  e/EndoFunctor
  (fmap [ev f]
    (Title. attr contents (f next)))

  e/Comonad
  (extract [_] contents))

(defprotocol HTML
  (html* [v]))

(defn html [v]
  (str "<html>\n"
       (html* v)
       "\n</html>"))

(extend-type Tag
  HTML
  (html* [ev]
    (str "<" (.tag ev) ">\n"
         (html* (.contents ev))
         "\n</" (.tag ev) ">")))

(extend-type P
  HTML
  (html* [ev]
    (str "<p>\n"
         (.contents ev)
         "\n</p>\n"
         (html* (.next ev)))))

(extend-type Title
  HTML
  (html* [ev]
    (str "<title>\n"
         (.contents ev)
         "\n</title>\n"
         (html* (.next ev)))))

(extend-type Pure
  HTML
  (html* [ev]
    (extract ev)))

(extend-type Free
  HTML
  (html* [ev]
    (let [v (extract ev)]
      (html* v))))

(defn tag [name]
  (fn
    ([] (liftF (Tag. name {} nil)))
    ([attr] (liftF (Tag. name attr nil)))))

(def head (tag "head"))
(def body (tag "body"))
(def h1 (tag "h1"))
(defn title [txt]
  (liftF (Title. {} txt nil)))
(defn p [txt]
  (liftF (P. {} txt nil)))

(def doc (for [_ (body)
               _ (p "this is some text")
               _ (p "and more text")]
           nil))

(prn :doc doc)
(print (html doc))
