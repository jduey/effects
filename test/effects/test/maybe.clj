; Copyright (c) Jim Duey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this
; distribution. By using this software in any fashion, you are
; agreeing to be bound by the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns effects.test.maybe
  (:refer-clojure :exclude [extend for])
  (:require [clojure.test :refer :all]
            [effects :refer :all]
            [effects.state :refer :all]
            [effects.maybe :refer :all]))

(defn maybe-f [n]
  (maybe (inc n)))

(defn maybe-g [n]
  (maybe (+ n 5)))

(deftest maybe-functor
  (is (= nothing (fmap nothing inc)))
  (is (= (maybe 2) (fmap (maybe 2) identity)))
  (is (= (maybe 5) (fmap (maybe 4) inc))))

(deftest maybe-applicative
  (is (= nothing (fapply list nothing (maybe 9))))
  (is (= nothing (fapply list (maybe 9) nothing)))
  (is (= (maybe [8 9]) (fapply list (maybe 8) (maybe 9)))))

(deftest first-law-maybe
  (is (= (flat-map (maybe 10) maybe-f)
         (maybe-f 10))))

(deftest second-law-maybe
  (is (= (flat-map (maybe 10) maybe)
         (maybe 10))))

(deftest third-law-maybe
  (is (= (flat-map (flat-map (maybe 5) maybe-f) maybe-g)
         (flat-map (maybe 5) (fn [x]
                               (flat-map (maybe-f x) maybe-g))))))

(defn maybe-co-f [n]
  (inc (extract n)))

(defn maybe-co-g [n]
  (+ (extract n) 5))

(deftest first-co-law-maybe
  (is (= (extend (maybe 10) extract)
         (maybe 10))))

(deftest second-co-law-maybe
  (is (= (extract (extend (maybe 7) maybe-co-f))
         (maybe-co-f (maybe 7)))))

(deftest third-co-law-maybe
  (is (= (extend (extend (maybe 2) maybe-co-f) maybe-co-f)
         (extend (maybe 2) (fn [x]
                             (maybe-co-f (extend x maybe-co-f)))))))

(deftest zero-law-maybe
  (is (= (flat-map (zero (maybe nil)) maybe-f)
         (zero (maybe nil))))
  (is (= (flat-map (maybe 4) (constantly (zero (maybe nil))))
         (zero (maybe nil))))
  (is (= (plus (maybe 6) (zero (maybe nil)))
         (maybe 6)))
  (is (= (plus (zero (maybe nil)) (maybe 6))
         (maybe 6))))


(def mm (plus maybe maybe))
(defn mm-f [n]
  (mm (inc n)))

(defn mm-g [n]
  (mm (+ n 5)))

(deftest mm-functor
  (is (= (zero (mm nil)) (fmap (zero (mm nil)) inc)))
  (is (= (mm 2) (fmap (mm 2) identity)))
  (is (= (mm 5) (fmap (mm 4) inc))))

(deftest mm-applicative
  (is (= (zero (mm nil)) (fapply list (zero (mm nil)) (mm 8))))
  (is (= (zero (mm nil)) (fapply list (mm 8) (zero (mm nil)))))
  (is (= (mm [8 9]) (fapply list (mm 8) (mm 9)))))

(deftest first-law-mm
  (is (= (flat-map (mm 10) mm-f)
         (mm-f 10))))

(deftest second-law-mm
  (is (= (flat-map (mm 10) mm)
         (mm 10))))

(deftest third-law-mm
  (is (= (flat-map (flat-map (mm 5) mm-f) mm-g)
         (flat-map (mm 5) (fn [x]
                               (flat-map (mm-f x) mm-g))))))

(deftest zero-law-mm
  (is (= (flat-map (zero (mm nil)) mm-f)
         (zero (mm nil))))
  (is (= (flat-map (mm 4) (constantly (zero (mm nil))))
         (zero (mm nil))))
  (is (= (plus (mm 6) (zero (mm nil)))
         (mm 6)))
  (is (= (plus (zero (mm nil)) (mm 6))
         (mm 6))))



(def sm (plus state maybe))
(defn sm-f [n]
  (sm (inc n)))

(defn sm-g [n]
  (sm (+ n 5)))

#_(deftest sm-functor
  (is (= (zero (sm nil)) (fmap (zero (sm nil)) inc)))
  (is (= (sm 2) (fmap (sm 2) identity)))
  (is (= (sm 5) (fmap (sm 4) inc))))

(deftest sm-applicative
  (is (= ((zero (sm nil)) :s)
         ((fapply list (zero (sm nil)) (sm 8)) :s)))
  (is (= ((zero (sm nil)) :s)
         ((fapply list (sm 8) (zero (sm nil))) :s)))
  (is (= ((sm [8 9]) :s)
         ((fapply list (sm 8) (sm 9)) :s))))

(deftest first-law-sm
  (is (= ((flat-map (sm 10) sm-f) :s)
         ((sm-f 10) :s))))

(deftest second-law-sm
  (is (= ((flat-map (sm 10) sm) :s)
         ((sm 10) :s))))

(deftest third-law-sm
  (is (= ((flat-map (flat-map (sm 5) sm-f) sm-g) :s)
         ((flat-map (sm 5) (fn [x]
                             (flat-map (sm-f x) sm-g))) :s))))

(deftest zero-law-sm
  (is (= ((flat-map (zero (sm nil)) sm-f) :s)
         ((zero (sm nil)) :s)))
  (is (= ((flat-map (sm 4) (constantly (zero (sm nil)))) :s)
         ((zero (sm nil)) :s)))
  (is (= ((plus (sm 6) (zero (sm nil))) :s)
         ((sm 6) :s)))
  (is (= ((plus (zero (sm nil)) (sm 6)) :s)
         ((sm 6) :s))))
