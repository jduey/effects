(ns effects.test.free
  (:refer-clojure :exclude [extend for])
  (:require [effects :refer :all]
            [effects.free :refer :all])
  (:import [effects.free Pure Free]))

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

(defn output [x] (liftF (Output. x nil)))
(def bell (liftF (Bell. nil)))
(def done (liftF (Done.)))

(def subr (for [_ (output :a)
                _ (output :b)]
            8))

(def prog (for [v subr
                _ bell
                _ done]
            nil))


(defprotocol ShowProg
  (show [_]))

(extend-type Output
  ShowProg
  (show [ev]
    (let [[v x] (extract ev)]
      (str "output " v \newline (show x)))))

(extend-type Bell
  ShowProg
  (show [ev]
    (let [x (extract ev)]
      (str "bell" \newline (show x)))))

(extend-type Done
  ShowProg
  (show [ev]
    (str "done" \newline)))

(extend-type Pure
  ShowProg
  (show [ev]
    (str "return" (extract ev) \newline)))

(extend-type Free
  ShowProg
  (show [ev]
    (show (extract ev))))

(println)
(prn :subr subr)
(println :prog)
(println  (show prog))

(defn pretty [x] (print (show x)))

(defn return [x]
  (Pure. x))

(extend-type clojure.lang.PersistentVector
  EndoFunctor
  (fmap [v f]
    (vec (map #(fmap % f) v))))

(extend-type Object
  EndoFunctor
  (fmap [v f]
    (f v))

  Monad
  (flat-map [v f]
    (extract (f v))))

(prn :x (for [x (Free. [1 2 3])]
          (inc x)))
