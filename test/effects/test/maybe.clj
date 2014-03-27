(ns effects.test.maybe
  (:refer-clojure :exclude [extend])
  (:require [clojure.test :refer :all]
            [effects :refer :all]
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

(deftest zero-law-maybe
  (is (= (flat-map (m-zero (maybe nil)) maybe-f)
         (m-zero (maybe nil))))
  (is (= (flat-map (maybe 4) (constantly (m-zero (maybe nil))))
         (m-zero (maybe nil))))
  (is (= (m-plus (maybe 6) (m-zero (maybe nil)))
         (maybe 6)))
  (is (= (m-plus (m-zero (maybe nil)) (maybe 6))
         (maybe 6))))


(def mm (ecomp maybe maybe))
(defn mm-f [n]
  (mm (inc n)))

(defn mm-g [n]
  (mm (+ n 5)))

(deftest mm-functor
  (is (= (m-zero (mm nil)) (fmap (m-zero (mm nil)) inc)))
  (is (= (mm 2) (fmap (mm 2) identity)))
  (is (= (mm 5) (fmap (mm 4) inc))))

(deftest mm-applicative
  (is (= (m-zero (mm nil)) (fapply list (m-zero (mm nil)) (mm 8))))
  (is (= (m-zero (mm nil)) (fapply list (mm 8) (m-zero (mm nil)))))
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
  (is (= (flat-map (m-zero (mm nil)) mm-f)
         (m-zero (mm nil))))
  (is (= (flat-map (mm 4) (constantly (m-zero (mm nil))))
         (m-zero (mm nil))))
  (is (= (m-plus (mm 6) (m-zero (mm nil)))
         (mm 6)))
  (is (= (m-plus (m-zero (mm nil)) (mm 6))
         (mm 6))))
