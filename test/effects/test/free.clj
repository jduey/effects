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
  (:import [effects.free Free FreeT Pure FreeA]))

(deftype Endo [x]
  Object
  (toString [_]
    (pr-str x))
  (equals [a b]
    (and (= (type b) Endo)
         (= x (.x b))))

  EndoFunctor
  (fmap [_ f]
    (Endo. (f x))))

(deftype AppFunc [x]
  Object
  (toString [_]
    (pr-str x))
  (equals [a b]
    (and (= (type b) AppFunc)
         (= x (.x b))))

  EndoFunctor
  (fmap [_ f]
    (AppFunc. (f x)))

  Applicative
  (wrap [_ new-v]
    (AppFunc. new-v))
  (fapply* [_ args]
    (AppFunc. (apply x (map #(.x %) args)))))

(defn liftEndo [ev]
  (AppFunc. (.x ev)))

(defn liftPure [v]
  (AppFunc. v))

(def eval-app-expr #(evaluate % liftPure liftEndo))

(deftest app-law-1
  (is (= (eval-app-expr (fapply inc (free (Endo. 4))))
         (eval-app-expr (fmap (free (Endo. 4)) inc)))))

(deftest app-law-2
  (is (= (eval-app-expr (free (Endo. 4)))
         (eval-app-expr (fapply identity (free (Endo. 4)))))))


(deftest app-law-3
  (is (= (eval-app-expr (fapply*
                     (fapply*
                      (fapply (fn [f1] (partial comp f1)) (free (Endo. (partial + 5))))
                      [(free (Endo. inc))])
                     [(free (Endo. 8))]))
         (eval-app-expr (fapply* (free (Endo. (partial + 5)))
                             [(fapply* (free (Endo. inc)) [(free (Endo. 8))])]))
         (eval-app-expr (fapply (partial + 5) (fapply inc (free (Endo. 8))))))))

(deftest app-law-4
  (is (= (eval-app-expr (fapply inc (pure 4)))
         (eval-app-expr (pure 5)))))

(deftest app-law-5
  (is (= (eval-app-expr (FreeA. (free (Endo. inc)) [(pure 4)] nil))
         (eval-app-expr (fapply #(% 4) (free (Endo. inc)))))))

(deftest multi-arg
  (is (= (AppFunc. 16)
         (eval-app-expr (fapply +
                            (pure 6)
                            (free (Endo. 7))
                            (fapply inc (free (Endo. 2))))))))

(defn fv [v]
  (free (Endo. (pure v))))

(defn fv-f [n]
  (fv (inc n)))

(defn fv-g [n]
  (fv (+ n 5)))

(def eval-monad-expr #(evaluate % identity
                                (fn eval-expr [ev]
                                  (evaluate (.x ev) identity eval-expr))))

(deftest first-law-fv
  (is (= (eval-monad-expr (flat-map (fv 10) fv-f))
         (eval-monad-expr (fv-f 10)))))

(deftest second-law-fv
  (is (= (eval-monad-expr (flat-map (fv 10) fv))
         (eval-monad-expr (fv 10)))))

(deftest third-law-fv
  (is (= (eval-monad-expr (flat-map (flat-map (fv 5) fv-f) fv-g))
         (eval-monad-expr (flat-map (fv 5) (fn [x]
                                             (flat-map (fv-f x) fv-g)))))))
