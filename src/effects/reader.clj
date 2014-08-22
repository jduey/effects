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

(deftype Reader [effect invoke-fn val-string]
  Object
  (toString [_]
    (if (= id effect)
      (str val-string)
      (str effect ": " val-string)))

  clojure.lang.IFn
  (invoke [_ s]
    (invoke-fn s))
  (applyTo [_ [v]]
    (invoke-fn v))

  Applicative
  (wrap [_ v]
    (Reader. effect
            (if (= id effect)
              (fn [s]
                v)
              (fn [s]
                (effect v)))
            (str v)))
  (fapply* [wrapped-f args]
    (if (= id effect)
      (flat-map wrapped-f #(comprehend % args))
      (throw (Exception. "hey there"))))

  Monad
  (flat-map [ev f]
    (Reader. effect
            (if (= id effect)
              (fn [s]
                ((f (ev s)) s))
              (fn [s]
                (flat-map (ev s)
                          (fn [v]
                            ((f v) s)))))
            (list "<flat-map>")))

  MonadZero
  (zero [_]
    (Reader. effect
            (fn [s] (zero (effect :nil)))
            (str (zero (effect :nil)))))
  (m-plus* [mv mvs]
    (Reader. effect
            (fn [s]
              (let [x (mv s)]
                (cond
                 (empty? mvs) x
                 (= (zero (effect :nil)) x) ((apply m-plus mvs) s)
                 :else x)))
            (list "<m-plus*>"))))

(def reader
  (reify
    Object
    (toString [_] (list "<Reader>"))

    clojure.lang.IFn
    (invoke [effect v]
      (Reader. id
               (fn [s] v)
               (str v)))

    Effects
    (ecomp* [effect effects]
      (let [e (if (empty? effects)
                id
                (apply ecomp effects))]
        (reify
          Object
          (toString [_]
            (if (= id e)
              "<Reader>"
              (str "<Reader " e ">")))

          clojure.lang.IFn
          (invoke [_ v]
            (Reader. e
                    (if (= e id)
                      (fn [s]
                        v)
                      (fn [s]
                        (e v)))
                    (str v))))))))

(defmethod print-method (type reader) [_ w]
  (.write w "#<Reader>"))

(defn read-context
  "Return a reader-monad value that returns the current context"
  []
  (->Reader id identity "<read-context>"))

(defn read-val
  "Return a reader-monad value that assumes the context to be a map and
   returns the value corresponding to the given key."
  [key]
  (flat-map (read-context)
            #(reader (get % key))))


(defn read-in-val [path & [default]]
  (flat-map (read-context)
            #(reader (get-in % path default))))
