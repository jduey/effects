; Copyright (c) Jim Duey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this
; distribution. By using this software in any fashion, you are
; agreeing to be bound by the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns effects.maybe
  (:refer-clojure :exclude [extend for])
  (:require [effects :refer :all]
            [effects.id :refer [id]]))

(declare maybe)

(def nothing
  (reify
    Object
    (toString [_] "<Nothing>")

    EndoFunctor
    (fmap [ev _] ev)

    Applicative
    (wrap [_ v]
      (maybe v))
    (fapply* [_ _] nothing)

    Monad
    (flat-map [_ _] nothing)

    Monoid
    (zero [_] nothing)
    (plus* [_ evs]
      (if (empty? evs)
        nothing
        (plus* (first evs) (rest evs))))))

(deftype MaybeT [e v]
  Object
  (equals [x y]
    (and (= (class x) (class y))
         (= v (extract y))))

  EndoFunctor
  (fmap [ev f]
    (flat-map ev (fn [x] (wrap ev (f x)))))

  Applicative
  (wrap [_ v]
    (if e
      (MaybeT. e (e (maybe v)))
      (MaybeT. nil v)))
  (fapply* [wrapped-f args]
    (flat-map wrapped-f #(comprehend % args)))

  Monad
  (flat-map [ev f]
    (if e
      (MaybeT. e (flat-map (extract ev)
                           (fn [x]
                             (if (= x nothing)
                               (e nothing)
                               (extract (f (extract x)))))))
      (let [ev (f v)]
        (if (= nothing ev)
          nothing
          (MaybeT. nil (extract ev))))))

  Monoid
  (zero [ev]
    (if e
      (MaybeT. e (e nothing))
      nothing))
  (plus* [mv mvs]
    (if e
      (MaybeT. e (flat-map (extract mv)
                           (fn [x]
                             (cond
                              (and (= x nothing) (empty? mvs)) (e nothing)
                              (= x nothing) (extract (apply plus mvs))
                              :else (e x)))))
      mv))

  Comonad
  (extract [_] v)
  (extend [ev f]
    (wrap ev (f ev))))

(def maybe
  (reify
    Object
    (toString [_]
      "<Maybe>")

    clojure.lang.IFn
    (invoke [_ v]
      (MaybeT. nil v))
    (applyTo [_ [v]]
      (maybe v))

    Monoid
    (plus* [effect effects]
      (if (empty? effects)
        maybe
        (let [e (apply plus effects)]
          (reify
            Object
            (toString [_]
              (str "<Maybe " e ">"))

            clojure.lang.IFn
            (invoke [_ v]
              (MaybeT. e (e (maybe v))))))))
    ))

(defmethod print-method (type maybe) [_ w]
  (.write w "#<Maybe>"))
