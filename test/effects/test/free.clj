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

  EndoFunctor
  (fmap [_ f]
    (Output. b (f next)))

  Comonad
  (extract [_]
    [b next]))

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
    "")

  EndoFunctor
  (fmap [_ _]
        (Done.)))

(defn output [x] (liftF (Output. x nil)))
(def bell (liftF (Bell. nil)))
(def done (liftF (Done.)))

(prn (flat-map bell (fn [_] done)))

(defprotocol ShowProg
  (show [_]))

(extend-type Output
  ShowProg
  (show [ev]
    (let [[v x] (extract ev)]
      (str "output " v \newline (evaluate x identity show)))))

(extend-type Bell
  ShowProg
  (show [ev]
    (let [x (extract ev)]
      (str "bell" \newline (evaluate x identity show)))))

(extend-type Done
  ShowProg
  (show [ev]
        (str "done" \newline)))


(def subr (for [_ (output :a)
                _ (output :b)]
            8))

(def prog (for [v subr
                _ bell
                _ done]
            nil))

(print (evaluate prog identity show))
