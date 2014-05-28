(ns effects.test.applicative
  (:refer-clojure :exclude [for extend])
  (:require [clojure.test :refer :all]
            [effects :refer :all]
            [effects.vector :refer :all]
            [effects.free :refer :all])
  (:import [effects.free FreeA]))

(deftest app-law-1
  (is (= (extract (fapply inc (free [4])))
         (extract (fmap (free [4]) inc)))))

(deftest app-law-2
  (is (= (extract (fapply identity (free [4])))
         (extract (free [4])))))

(deftest app-law-3
  (is (= (extract (fapply*
                   (fapply*
                    (fapply (fn [f1] (partial comp f1)) (free [(partial + 5)]))
                    [(free [inc])])
                   [(free [8])]))
         (extract (fapply* (free [(partial + 5)])
                           [(fapply* (free [inc]) [(free [8])])])))))

(deftest app-law-4
  (is (= (extract (fapply inc (pure 4)))
         (extract (pure 5)))))

(deftest app-law-5
  #_(is (= (extract (FreeA. (free [inc]) [(pure 4)] nil))
         (extract (FreeA. (pure #(% 4)) [(free [inc])] nil)))))

(run-tests)


