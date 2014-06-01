(ns effects.test.applicative
  (:refer-clojure :exclude [for extend])
  (:require [clojure.test :refer :all]
            [effects :refer :all]
            [effects.vector :refer :all]
            [effects.free :refer :all])
  (:import [effects.free FreeA Pure Free]))

(deftype Endo [x]
  Object
  (toString [_] (pr-str x))
  (equals [a b]
    (and (= (type b) Endo)
         (= x (extract b))))

  EndoFunctor
  (fmap [_ f]
    (Endo. (f x)))

  Applicative
  (wrap [_ v]
    (Endo. v))
  (fapply* [_ args]
    (Endo. (apply x (map extract args))))

  Comonad
  (extract [_] x))

(def eval-expr #(evaluate % ->Endo identity))

(deftest app-law-1
  (is (= (eval-expr (fapply inc (free (Endo. 4))))
         (eval-expr (fmap (free (Endo. 4)) inc)))))

(deftest app-law-2
  (is (= (eval-expr (fapply identity (free (Endo. 4))))
         (eval-expr (free (Endo. 4))))))


(deftest app-law-3
  (is (= (eval-expr (fapply*
                     (fapply*
                      (fapply (fn [f1] (partial comp f1)) (free (Endo. (partial + 5))))
                      [(free (Endo. inc))])
                     [(free (Endo. 8))]))
         (eval-expr (fapply* (free (Endo. (partial + 5)))
                             [(fapply* (free (Endo. inc)) [(free (Endo. 8))])]))
         (eval-expr (fapply (partial + 5) (fapply inc (free (Endo. 8))))))))

(deftest app-law-4
  (is (= (eval-expr (fapply inc (pure 4)))
         (eval-expr (pure 5)))))

(deftest app-law-5
  (is (= (eval-expr (FreeA. (free (Endo. inc)) [(pure 4)] nil))
         (eval-expr (fapply #(% 4) (free (Endo. inc)))))))

(deftest multi-arg
  (is (= (Endo. 16)
         (eval-expr (fapply +
                            (pure 6)
                            (free (Endo. 7))
                            (fapply inc (free (Endo. 2))))))))

(run-tests)

(println)
(def b (fmap (free (Endo. :bogus)) vector))
#_(prn b)
(prn (eval-expr b))

(println)
(def b (fapply inc (pure 9)))
#_(prn b)
(prn (eval-expr b))

(println)
(def b (fapply + (pure 9) (pure 5)))
#_(prn b)
(prn (eval-expr b))

(println)
(def b (fapply + (pure 9) (pure 5) (pure 100)))
#_(prn b)
(prn (eval-expr b))

(println)
(def b (fapply inc (free (Endo. 8))))
#_(prn b)
(prn (eval-expr b))

(println)
(def b (fapply + (free (Endo. 100)) (free (Endo. 8)) (free (Endo. 7))))
#_(prn b)
(prn (eval-expr b))

(println)
(def b (fapply + (free (Endo. 100)) (pure 8) (free (Endo. 7))))
#_(prn b)
(prn (eval-expr b))

(def eval-expr #(evaluate % ->Endo (fn this-fn [ev]
                                     (evaluate (extract ev) ->Endo this-fn))))

(println)
(def b (for [x (free (Endo. (pure 8)))
             y (free (Endo. (pure 3)))]
         (pure (+ x y))))
(prn b)
(prn (eval-expr b))
