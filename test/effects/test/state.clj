; Copyright (c) Jim Duey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this
; distribution. By using this software in any fashion, you are
; agreeing to be bound by the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns effects.test.state
  (:refer-clojure :exclude [extend])
  (:require [clojure.test :refer :all]
            [effects :refer :all]
            [effects.state :refer :all]))

(defn state-f [n]
  (state (inc n)))

(defn state-g [n]
  (state (+ n 5)))

(deftest first-law-state
  (let [mv1 (flat-map (state 10) state-f)
        mv2 (state-f 10)]
    (is (= (mv1 {}) (mv2 {})))))

(deftest second-law-state
  (let [mv1 (flat-map (state 10) state)
        mv2 (state 10)]
    (is (= (mv1 :state) (mv2 :state)))))

(deftest third-law-state
  (let [mv1 (flat-map (flat-map (state 4) state-f) state-g)
        mv2 (flat-map (state 4)
                    (fn [x]
                      (flat-map (state-f x) state-g)))]
    (is (= (mv1 :state) (mv2 :state)))))

(deftest test-update-state
  (is (= [:state :new-state]
         ((update-state (constantly :new-state)) :state))))

(deftest test-get-val
  (is (= [17 {:a 17}]
         ((get-val :a) {:a 17}))))

(deftest test-set-val
  (is (= [17 {:a 12}]
         ((set-val :a 12) {:a 17}))))

(deftest test-update-val
  (is (= [5 {:a 19}]
         ((update-val :a + 14) {:a 5}))))

(deftest test-get-in-val
  (let [state {:a {:b 1} :c {:d {:e 2}}}]
    (are [expected args] (= expected ((apply get-in-val args) state))
         ;;
         [1 state] [[:a :b]]
         ;;
         [:def state] [[:z] :def]
         ;; Not working: should be fixed on the next cljs release
         ;; already reported in
         ;; http://dev.clojure.org/jira/browse/CLJS-464
         ;; [nil state] [[:a :b :c]]
         ;;
         [2 state] [[:c :d :e]]
         ;;
         [{:b 1} state] [[:a]])))

(deftest test-assoc-in-val
  (is (= [nil {:a {:b {:c 9}}}]
         ((assoc-in-val [:a :b :c] 9) {}))))

(deftest test-update-in-val
  (are [expected in-state path args] (= expected
                                        ((apply update-in-val path args) in-state))
       [2 {:a {:b 4}}]      {:a {:b 2}}  [:a :b]  [* 2]
       [2 {:a {:b 3}}]      {:a {:b 2}}  [:a :b]  [inc]
       [nil {:a {:b [1]}}]  {:a nil}     [:a :b]  [(fnil conj []) 1]))
