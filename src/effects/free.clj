(ns effects.free
  (:refer-clojure :exclude [extend for])
  (:require [effects :refer :all]))

(deftype Pure [v]
  Object
  (toString [_]
    (pr-str v))

  Applicative
  (wrap [_ v]
    (Pure. v))

  Monad
  (flat-map [ev f]
    (f v))

  Comonad
  (extract [_] v))


(deftype Free [val]
  Object
  (toString [_]
    (pr-str val))

  Applicative
  (wrap [_ v]
    (Pure. v))

  Monad
  (flat-map [_ f]
    (Free. (fmap val #(flat-map % f))))

  Comonad
  (extract [_]
    val))


(defn liftF [f-val]
  (Free. (fmap f-val (fn [x] (Pure. x)))))
