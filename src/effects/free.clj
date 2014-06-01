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

(declare free-app)
(declare free-plus)

(defprotocol FreeProto
  (evaluate [_ pure lift]))

(deftype Pure [v meta]
  Object
  (toString [_]
    (pr-str v))

  clojure.lang.IMeta
  (meta [_] meta)

  FreeProto
  (evaluate [_ pure _]
    (pure v))

  EndoFunctor
  (fmap [_ f]
    (prn :pure v f)
    (Pure. (f v) nil))

  Applicative
  (wrap [_ v]
    (Pure. v nil))
  (fapply* [f args]
    (free-app f args))

  Monad
  (flat-map [_ f]
    (f v)))

(defn pure [v]
  (Pure. v nil))

(deftype FreeZero []
  Object
  (toString [_] "")

  Applicative
  (wrap [_ v]
    (pure v))

  Monoid
  (zero [fz] fz)
  (plus* [_ evs] evs))

(def free-zero (FreeZero.))

(deftype FreeA [f args meta]
  Object
  (toString [_]
    (pr-str f args))

  clojure.lang.IObj
  (withMeta [_ m] (FreeA. f args m))

  clojure.lang.IMeta
  (meta [_] meta)

  FreeProto
  (evaluate [_ pure lift]
    (fapply* (evaluate f pure lift) (map #(evaluate % pure lift) args)))

  EndoFunctor
  (fmap [_ pure-f]
    (FreeA. (fmap f #(comp pure-f %)) args nil))

  Applicative
  (wrap [_ v]
    (pure v))
  (fapply* [f args]
    (FreeA. f args nil))

  Monoid
  (zero [_] free-zero)
  (plus* [v vs]
    (free-plus (cons v vs))))

(defn free-app [f x]
  (FreeA. f x nil))

(deftype FreePlus [alts meta]
  Object
  (toString [_]
    (pr-str alts))

  clojure.lang.IObj
  (withMeta [_ m] (FreePlus. alts m))

  clojure.lang.IMeta
  (meta [_] meta)

  EndoFunctor
  (fmap [_ pure-f]
    (free-plus (map #(fmap % pure-f) alts)))

  Applicative
  (wrap [_ v]
    (pure v))
  (fapply* [f args]
    (free-app f args))

  Monoid
  (zero [_] free-zero)
  (plus* [v vs]
    (free-plus (cons v vs))))

(defn free-plus [alts]
  (FreePlus. alts nil))

(deftype Free [v meta]
  Object
  (toString [_]
    (pr-str v))

  clojure.lang.IMeta
  (meta [_] meta)

  FreeProto
  (evaluate [_ _ lift]
    (lift v))

  EndoFunctor
  (fmap [_ f]
    (Free. (fmap v f) nil))

  Applicative
  (wrap [_ new-v]
    (Free. (fmap v (constantly new-v)) nil))
  (fapply* [f args]
    (free-app f args))

  Monad
  (flat-map [_ f]
    (Free. (fmap v #(flat-map % f)) nil))

  Monoid
  (zero [_] free-zero)
  (plus* [v vs]
    (free-plus (cons v vs))))

(defn free [v]
  (Free. v nil))

(defn liftF [f-val]
  (free (fmap f-val (fn [x] (pure x)))))


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
                 #_(map extract)
                 (apply plus)))))

(defn liftFT [f-val]
  (FreeT. (fmap f-val (fn [x] (pure (wrap f-val x))))))
