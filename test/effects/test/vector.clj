; Copyright (c) Jim Duey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this
; distribution. By using this software in any fashion, you are
; agreeing to be bound by the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns effects.test.vector
  (:refer-clojure :exclude [extend for])
  (:require [clojure.test :refer :all]
            [effects :as e :refer [flat-map zero plus extract wrap for]]
            [effects.vector :refer :all]))

(defn vector-f [n]
  (vector (inc n)))

(defn vector-g [n]
  (vector (+ n 5)))

(deftest first-law-vector
  (is (= (flat-map [10] vector-f)
         (vector-f 10))))

(deftest second-law-vector
  (is (= (flat-map [10] vector)
         [10])))

(deftest third-law-vector
  (is (= (flat-map (flat-map [4 9] vector-f) vector-g)
         (flat-map [4 9] (fn [x]
                         (flat-map (vector-f x) vector-g))))))

(deftest zero-law-vector
  (is (= (flat-map [] vector-f)
         []))
  (is (= (flat-map [4] (constantly []))
         []))
  (is (= (plus (vector 5 6) [])
         (vector 5 6)))
  (is (= (plus [] (vector 5 6))
         (vector 5 6))))
