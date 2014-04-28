; Copyright (c) Jim Duey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this
; distribution. By using this software in any fashion, you are
; agreeing to be bound by the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns effects.reader
  (:refer-clojure :exclude [extend for])
  (:require [effects :refer :all]
            [effects.id :refer [id]]))

(declare reader)

(deftype Reader [invoke-fn]
  clojure.lang.IFn
  (invoke [_ s]
    (invoke-fn s))
  (applyTo [_ [v]]
    (invoke-fn v))

  Applicative
  (wrap [_ v]
    (Reader. (constantly v)))

  Monad
  (flat-map [ev f]
    (Reader. (fn [s]
               (let [v (ev s)]
                 ((f v) s))))))

(defn reader [v]
  (Reader. (constantly v)))

(def read-environment (Reader. identity))

(defn read-val [key]
  (flat-map read-environment
            #(reader (get % key))))
