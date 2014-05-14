;; Copyright (c) Jim Duey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution. By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns effects.free
  (:refer-clojure :exclude [extend for seq])
  (:require [effects :refer :all]))

(declare free)

(declare freeA)

(deftype Pure [v]
  Object
  (toString [_]
    (pr-str v))

  EndoFunctor
  (fmap [_ f]
    (Pure. (f v)))

  Applicative
  (wrap [_ v]
    (Pure. v))
  (fapply* [_ args]
    (if (= 1 (count args))
      (freeA (fmap v #(partial apply %))
             (fmap (first args) list))
      (freeA (fmap v #(partial apply %))
             (apply fapply
                    (fmap (first args) (fn [arg]
                                         (fn [& xs]
                                           (cons arg xs))))
                    (rest args)))))

  Monad
  (flat-map [_ f]
    (f v))

  Comonad
  (extract [_] v))

(deftype FreeA [h x]
  Object
  (toString [_]
    (pr-str h x))

  EndoFunctor
  (fmap [_ f]
    (FreeA. (fmap h #(comp f %)) x))

  Applicative
  (wrap [_ v]
    (Pure. v))
  (fapply* [_ [y]]
    (FreeA. (fmap h #(partial apply %))
            (FreeA. (fmap x #(partial cons %))
                    (fmap y list))))

  Monoid
  (zero [_]
    )
  (plus* [v vs]
    ))

(defn freeA [f x]
  (FreeA. f x))

(deftype Free [v]
  Object
  (toString [_]
    (pr-str v))

  EndoFunctor
  (fmap [_ f]
    (Free. (fmap v f)))

  Applicative
  (wrap [_ new-v]
    (Free. new-v))
  (fapply* [_ args]
    (if (= 1 (count args))
      (freeA (fmap v #(partial apply %))
             (fmap (first args) list))
      (freeA (fmap v #(partial apply %))
             (apply fapply
                    (fmap (extract (first args))
                          (fn [arg]
                            (fn [& xs]
                              (cons arg xs))))
                    (rest args)))))

  Monad
  (flat-map [_ f]
    (Free. (fmap v #(flat-map % f))))

  Comonad
  (extract [_] v))

(defn free [v]
  (Free. v))

(defn liftF [f-val]
  (Free. (fmap f-val (fn [x] (Pure. x)))))


(deftype FreeT [mv]
  Object
  (toString [_]
    (pr-str mv))

  Applicative
  (wrap [_ v]
    (FreeT. (wrap mv v)))


  Monad
  (flat-map [ev f]
    (FreeT. (flat-map mv (fn [x]
                           (wrap mv (fmap x (fn [ev] (flat-map ev f))))))))

  Monoid
  (zero [_]
    (FreeT. (zero (wrap mv :nil))))
  (plus* [mv mvs]
    (FreeT. (->> (cons mv mvs)
                 (map extract)
                 (apply plus))))

  Comonad
  (extract [_] mv))

(defn liftFT [f-val]
  (FreeT. (fmap f-val (fn [x] (Pure. (wrap f-val x))))))
