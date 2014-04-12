; Copyright (c) Jim Duey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this
; distribution. By using this software in any fashion, you are
; agreeing to be bound by the terms of this license.
; You must not remove this notice, or any other, from this software.

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
