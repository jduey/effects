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
            [effects :refer :all]
            [effects.free :refer :all])
  (:import [effects.free Pure Free FreeT]))

(extend-type clojure.lang.PersistentVector
  Applicative
  (wrap [_ v]
    [v])

  Monad
  (flat-map [vs f]
    (vec (mapcat f vs)))

  MonadZero
  (zero [_] [])
  (plus* [mv mvs] (vec (apply concat mv mvs))))

(println "------------")

(deftype Output [b next]
  Object
  (toString [_]
    (pr-str b next))

  EndoFunctor
  (fmap [_ f]
    (Output. b (f next)))

  Comonad
  (extract [_] [b next]))

(deftype Bell [next]
  Object
  (toString [_]
    (pr-str next))

  EndoFunctor
  (fmap [_ f]
    (Bell. (f next)))

  Comonad
  (extract [_] next))

(deftype Done []
  Object
  (toString [_]
    "Done")

  EndoFunctor
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
      (flat-map (show x)
                (fn [s]
                  (wrapper (str "output " v \newline s)))))))

(extend-type Bell
  ShowProg
  (show* [ev wrapper]
    (flat-map (show (extract ev))
              (fn [s]
                (wrapper (str "bell" \newline s))))))

(extend-type Done
  ShowProg
  (show* [ev wrapper]
    (wrapper (str "done" \newline))))

(extend-type FreeT
  ShowProg
  (show* [ev _]
    (let [v (extract ev)]
      (flat-map v #(show* % (partial wrap v))))))

(extend-type Pure
  ShowProg
  (show* [ev wrapper]
    (let [v (extract ev)]
      (flat-map v
                (fn [x]
                  (wrap v (str "return " x \newline)))))))

(defn pretty [x] (print (show x)))

(defn return [x]
  (Pure. x))

#_(extend-type Object
  EndoFunctor
  (fmap [v f]
    (f v))

  Monad
  (flat-map [v f]
    (extract (f v))))


(def subr (plus (output :a)
                (output :b)))

(prn :subr subr)

(def prog (for [_ subr
                ;; :let [_ (prn :prog-v v)]
                _ bell
                #_ done]
            :bogus))

(prn :prog prog)
(println)

(prn (show prog))
#_(pretty prog)
