; Copyright (c) Jim Duey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this
; distribution. By using this software in any fashion, you are
; agreeing to be bound by the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns effects.test.reader
  (:refer-clojure :exclude [extend for])
  (:require [clojure.test :refer :all]
            [effects :as e :refer [flat-map zero plus extract wrap for]]
            [effects.reader :refer :all]))

(defn reader-f [n]
  (reader (inc n)))

(defn reader-g [n]
  (reader (+ n 5)))

(deftest first-law-reader
  (is (= ((flat-map (reader 10) reader-f) :env)
         ((reader-f 10) :env))))

(deftest second-law-reader
  (is (= ((flat-map (reader 10) reader) :env)
         ((reader 10) :env))))

(deftest third-law-reader
  (is (= ((flat-map (flat-map (reader 3) reader-f) reader-g) :env)
         ((flat-map (reader 3)
                    (fn [x]
                      (flat-map (reader-f x) reader-g))) :env))))
