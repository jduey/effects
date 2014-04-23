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
  (plus* [mv mvs] (apply concat mv mvs)))

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

(def subr (for [_ (output :a)
                _ (output :b)]
            8))

(prn :subr subr)

(defprotocol ShowProg
  (show [v wrapper]))

(extend-type Output
  ShowProg
  (show [ev _]
    (let [[v x] (extract ev)]
      (for [s (show x _)]
        (str "output " v \newline s)))))

(extend-type Bell
  ShowProg
  (show [ev _]
    (for [s (show (extract ev) _)]
      (str "bell" \newline s))))

(extend-type Done
  ShowProg
  (show [ev wrapper]
    (wrapper (str "done" \newline))))

(extend-type FreeT
  ShowProg
  (show [ev _]
    (let [v (extract ev)]
      (flat-map v #(show % (partial wrap v))))))

(extend-type Pure
  ShowProg
  (show [ev _]
    (for [v (extract ev)]
      (str "return " v \newline))))

(defn pretty [x] (print (show x identity)))

(defn return [x]
  (Pure. x))

#_(extend-type Object
  EndoFunctor
  (fmap [v f]
    (f v))

  Monad
  (flat-map [v f]
    (extract (f v))))



(def prog (for [v subr
                ;; :let [_ (prn :prog-v v)]
                _ bell
                _ done]
            :bogus))

(prn :prog prog)
(println)

(prn (show prog identity))
(pretty prog)
