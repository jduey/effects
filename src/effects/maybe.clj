(ns effects.maybe
  (:refer-clojure :exclude [extend])
  (:require [effects :refer :all]
            [effects.id :refer [id]]))

(declare maybe)

(def nothing
  (reify
    Object
    (toString [_] "<Nothing>")

    Functor
    (fmap [ev _] ev)

    Applicative
    (wrap [_ v]
      (maybe v))
    (fapply* [_ _] nothing)

    Monad
    (flat-map [_ _] nothing)

    MonadZero
    (m-zero [_] nothing)
    (m-plus* [_ evs]
      (if (empty? evs)
        nothing
        (m-plus* (first evs) (rest evs))))))

(deftype MaybeT [e v]
  Object
  (equals [x y]
    (and (= (class x) (class y))
         (= v (extract y))))

  Functor
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

  MonadZero
  (m-zero [ev]
    (if e
      (MaybeT. e (e nothing))
      nothing))
  (m-plus* [mv mvs]
    (if e
      (MaybeT. e (flat-map (extract mv)
                           (fn [x]
                             (cond
                              (and (= x nothing) (empty? mvs)) (e nothing)
                              (= x nothing) (extract (apply m-plus mvs))
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

    Effects
    (ecomp* [effect effects]
      (if (empty? effects)
        maybe
        (let [e (apply ecomp effects)]
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
