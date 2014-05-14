; Copyright (c) Jim Duey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this
; distribution. By using this software in any fashion, you are
; agreeing to be bound by the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns effects.id
  (:require [effects :as eff]))

(def id
  (reify
    ;; Eq
    ;; (=* [x y] (type= x y))

    ;; Stringable
    ;; (string-list [_]
    ;;              (list "<Id> "))

    eff/Monoid
    (plus* [effect effects]
      (if (empty? effects)
        id
        (eff/plus* (first effects) (rest effects))))

    clojure.lang.IFn
    (invoke [_ v]
      (reify
        ;; Stringable
        ;; (string-list [_]
        ;;   (comp (list "<Id: ")
        ;;         (string-list v)
        ;;         (list ">")))

        ;; Monoid
        ;; (comp* [effect effects]
        ;;   (cond
        ;;    (empty? effects) effect
        ;;    (apply comp effects)))

        eff/Applicative
        (wrap [ev v] (recur v))

        eff/Monad
        (flat-map [mv f] (f v))

        eff/Comonad
        (extract [wv] v)))))
