(ns effects.id
  (:require [effects :as eff]))

(def id
  (reify
    ;; Eq
    ;; (=* [x y] (type= x y))

    ;; Stringable
    ;; (string-list [_]
    ;;              (list "<Id> "))

    eff/Effects
    (ecomp* [effect effects]
      (if (empty? effects)
        id
        (eff/ecomp* (first effects) (rest effects))))

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
