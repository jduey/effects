; Copyright (c) Jim Duey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this
; distribution. By using this software in any fashion, you are
; agreeing to be bound by the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns effects.test.free
  (:refer-clojure :exclude [extend for])
  (:require [clojure.test :refer :all]
            [effects :as e :refer [flat-map zero plus extract wrap for fmap]]
            [effects.free :refer :all]
            [effects.reader :refer :all]
            [effects.vector :refer :all])
  (:import [effects.free Free FreeT Pure]))

#_(extend-type java.lang.Long
  e/EndoFunctor
  (fmap [n f]
    (f n))

  e/Monad
  (flat-map [n f]
    (f n)))

#_(extend-type java.lang.String
  e/Monad
  (flat-map [n f]
    (f n)))

#_(defn fv [v]
  (FreeT. vector [v]))

#_(defn fv-f [n]
  (fv (inc n)))

#_(defn fv-g [n]
  (fv (+ n 5)))

#_(deftest first-law-fv
  (is (= (flat-map (fv 10) fv-f)
         (fv-f 10))))

#_(deftest second-law-fv
  (is (= (flat-map (fv 10) fv)
         (fv 10))))

#_(deftest third-law-fv
  (is (= (flat-map (flat-map (fv 5) fv-f) fv-g)
         (flat-map (fv 5) (fn [x]
                               (flat-map (fv-f x) fv-g))))))

#_(deftest zero-law-fv
  (is (= (flat-map (zero (fv nil)) fv-f)
         (zero (fv nil))))
  (is (= (flat-map (fv 4) (constantly (zero (fv nil))))
         (zero (fv nil))))
  (is (= (plus (fv 6) (zero (fv nil)))
         (fv 6)))
  (is (= (plus (zero (fv nil)) (fv 6))
         (fv 6))))

#_(run-tests)

(println)
(println "------------")

(deftype Output [b next]
  Object
  (toString [_]
    (pr-str b next))

  e/EndoFunctor
  (fmap [_ f]
    (Output. b (f next)))

  e/Comonad
  (extract [_] [b next]))

(deftype Bell [next]
  Object
  (toString [_]
    (pr-str next))

  e/EndoFunctor
  (fmap [_ f]
    (Bell. (f next)))

  e/Comonad
  (extract [_] next))

(deftype Done []
  Object
  (toString [_] "")

  e/EndoFunctor
  (fmap [_ _]
    (Done.)))

#_(defn output [x] (liftF (Output. x nil)))
(defn output [x] (liftFT vector (Output. x :output-next)))

#_(def bell (liftF (Bell. nil)))
(def bell (liftFT vector (Bell. :bell-next)))

#_(def done (liftF (Done.)))
(def done (liftFT vector (Done.)))

(defprotocol ShowProg
  (show* [v wrapper]))

(defn show [v]
  (show* v identity))

(extend-type Output
  ShowProg
  (show* [ev wrapper]
    (let [[v x] (extract ev)]
      (flat-map (show* x wrapper)
                (fn [s]
                  (wrapper (str "output " v \newline s)))))))

(extend-type Bell
  ShowProg
  (show* [ev wrapper]
    (flat-map (show* (extract ev) wrapper)
              (fn [s]
                (wrapper (str "bell" \newline s))))))

(extend-type Done
  ShowProg
  (show* [ev wrapper]
    (wrapper (str "done" \newline))))

(extend-type Free
  ShowProg
  (show* [ev _]
    (let [v (extract ev)]
      (show* v identity))))

(extend-type FreeT
  ShowProg
  (show* [ev _]
    (let [v (extract ev)]
      (flat-map v #(show* % (partial wrap v))))))

(extend-type Pure
  ShowProg
  (show* [ev wrapper]
    (wrapper (str "return " (extract ev) \newline))))

(defn pretty [x] (print (show x)))

(defn return [x]
  (Pure. x))

#_(extend-type Object
  e/EndoFunctor
  (fmap [v f]
    (f v))

  e/Monad
  (flat-map [v f]
    (extract (f v))))


#_(def subr (for [_ (plus (output :a)
                        (output :b))]
            8))

#_(def subr (for [_ (output (output :a))
                _ (output :b)]
            8))

#_(prn :subr subr)

#_(def prog (for [v subr
                _ bell
                _ (output v)
                _ done]
            :bogus))

#_(prn :prog prog)
#_(println)

#_(prn (show prog))
#_(pretty prog)


(deftype Tag [tag attr contents next]
  Object
  (toString [_]
    (pr-str tag attr contents next))

  e/EndoFunctor
  (fmap [_ f]
    (Tag. tag attr contents (f next)))

  e/Comonad
  (extract [_] tag))

(defprotocol HTML
  (html* [v wrapper]))

(defn to-html [v]
  (html* v identity))

(extend-type java.lang.String
  HTML
  (html* [ev wrapper]
    (wrapper ev)))

(extend-type Tag
  HTML
  (html* [ev wrapper]
    (for [contents (html* (.contents ev) wrapper)
          next (html* (.next ev) wrapper)]
      (let [next (if (= next "")
                   ""
                   (str \newline next))]
        (str "<" (.tag ev) ">\n"
             contents
             "\n</" (.tag ev) ">"
             next)))))

(extend-type Pure
  HTML
  (html* [ev wrapper]
    (wrapper "")))

#_(extend-type Free
  HTML
  (html* [ev]
    (let [v (extract ev)]
      (html* v))))

(extend-type FreeT
  HTML
  (html* [ev _]
    (let [v (extract ev)]
      (flat-map v #(html* % (partial wrap v))))))

(defn tag [name]
  (fn [attr & contents]
    (let [guts (if (empty? contents)
                 (Pure. nil)
                 (reduce (fn [ev next]
                           (flat-map ev (fn [_] next)))
                         contents))]
      (liftFT reader (Tag. name attr guts nil)))))

(defn insert [k]
  (FreeT. reader (read-val k)))

(def html (tag "html"))
(def head (tag "head"))
(def body (tag "body"))
(def h1 (tag "h1"))
(def p (tag "p"))
(def title (tag "title"))

(def body (body {}
                (p {} "this is some text")
                (p {} (insert :second-para))))

(def doc (html {}
               (head {} (title {} (insert :title)))
               body))

(prn :doc doc)
(print ((to-html doc) {:title "This is the title"
                       :second-para "yet more text"}))
