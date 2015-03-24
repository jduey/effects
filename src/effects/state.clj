; Copyright (c) Jim Duey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this
; distribution. By using this software in any fashion, you are
; agreeing to be bound by the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns effects.state
  (:refer-clojure :exclude [extend for])
  (:require [effects :refer :all]
            [effects.id :refer [id]]))

(declare state)

(deftype State [effect invoke-fn val-string]
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
    (State. effect
            (if (= id effect)
              (fn [s]
                (list v s))
              (fn [s]
                (effect (list v s))))
            (str v)))
  (fapply* [wrapped-f args]
    (if (= id effect)
      (flat-map wrapped-f #(comprehend % args))
      (State. effect
              (fn [s]
                ((flat-map (State. effect (fn [s]
                                            (effect [(fn [wrapped-f & args]
                                                       (apply wrapped-f args))
                                                     s])) "")
                           #(comprehend % (cons wrapped-f args)))
                 s))
              (list "<fapply*>"))))

  Monad
  (flat-map [ev f]
    (State. effect
            (if (= id effect)
              (fn [s]
                (let [[v ss] (ev s)]
                  ((f v) ss)))
              (fn [s]
                (flat-map (ev s)
                          (fn [[v ss]]
                            ((f v) ss)))))
            (list "<flat-map>")))

  Monoid
  (zero [_]
    (State. effect
            (fn [s] (zero (effect :nil)))
            (str (zero (effect :nil)))))
  (plus* [mv mvs]
    (State. effect
            (fn [s]
              (let [x (mv s)]
                (cond
                 (empty? mvs) x
                 (= (zero (effect :nil)) x) ((apply plus mvs) s)
                 :else x)))
            (list "<plus*>"))))

(def state
  (reify
    Object
    (toString [_] (list "<State>"))

    clojure.lang.IFn
    (invoke [effect v]
      (State. id
              (fn [s] (list v s))
              (str v)))
    (applyTo [_ [v]]
      (State. id
              (fn [s] (list v s))
              (str v)))

    Monoid
    (plus* [effect effects]
      (let [e (if (empty? effects)
                id
                (apply plus effects))]
        (reify
          Object
          (toString [_]
            (if (= id e)
              "<State>"
              (str "<State " e ">")))

          clojure.lang.IFn
          (applyTo [_ [v]]
            (State. e
                    (if (= e id)
                      (fn [s]
                        (list v s))
                      (fn [s]
                        (e (list v s))))
                    (str v)))
          (invoke [_ v]
            (State. e
                    (if (= e id)
                      (fn [s]
                        (list v s))
                      (fn [s]
                        (e (list v s))))
                    (str v))))))))

(defmethod print-method (type state) [_ w]
  (.write w "#<State>"))

(defn update-state [f]
  (->State id
           (fn [s]
             (list s (f s)))
           "<update-state>"))

(defn set-state
  "Return a state-monad value that replaces the current state by s and
   returns the previous state."
  [s]
  (update-state (constantly s)))

(defn get-state
  "Return a state-monad value that returns the current state and does not
   modify it."
  []
  (update-state identity))

(defn get-val
  "Return a state-monad value that assumes the state to be a map and
   returns the value corresponding to the given key. The state is not modified."
  [key & [default]]
  (flat-map (get-state)
            #(state (get % key default))))


(defn update-val
  "Return a state-monad value that assumes the state to be a map and
   replaces the value associated with the given key by the return value
   of f applied to the old value and args. The old value is returned."
  [key f & args]
  (flat-map (update-state #(apply update-in % [key] f args))
            #(state (get % key))))

(defn set-val
  "Return a state-monad value that assumes the state to be a map and
   replaces the value associated with key by val. The old value is returned."
  [key val]
  (update-val key (constantly val)))

(defn get-in-val [path & [default]]
  (flat-map (get-state)
            #(state (get-in % path default))))

(defn assoc-in-val [path val]
  (flat-map (update-state #(assoc-in % path val))
            #(state (get-in % path))))

(defn update-in-val [path f & args]
  (flat-map (update-state #(apply update-in % path f args))
            #(state (get-in % path))))
