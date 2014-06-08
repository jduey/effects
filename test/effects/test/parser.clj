(ns effects.test.parser
  (:refer-clojure :exclude [for seq extend])
  (:require [effects :refer :all]
            [effects.free :refer :all]
            [effects.maybe :refer :all]
            [effects.state :refer :all])
  (:import [effects.free Pure Free FreeA FreePlus FreeZero]))

(def parser (plus state maybe))
(defn parser-set-val [k v]
  (->State maybe
           (fn [s] (maybe (list v (assoc s k v))))
           (pr-str "parser-set-val" k v)))

(deftype EBNF [x]
  Applicative
  (fapply* [_ args]
    (EBNF. (fapply* (parser (fn [& args]
                              (apply str (interpose " , " args))))
                    (map #(.x %) args))))

  Monoid
  (plus* [v vs]
    (EBNF. (fapply* (parser (fn [& args]
                              (apply str (interpose " | " args))))
                    (map #(.x %) (cons v vs))))))

(defn term [v]
  (pure v))

(defn pure-ebnf [v]
  (EBNF. (parser (condp = v
                   \newline "\"\\n\""
                   \tab "\"\\t\""
                   \space "\" \""
                   \formfeed "\"\\f\""
                   (format "\"%s\"" v)))))

(defprotocol GenEBNF
  (gen-ebnf [_]))

(deftype Rule [name expr]
  EndoFunctor
  (fmap [_ f] (Rule. name (fmap expr f)))

  GenEBNF
  (gen-ebnf [_]
    (EBNF. (for [expr-ebnf (.x (evaluate expr pure-ebnf gen-ebnf))
                 _ (parser-set-val name (str expr-ebnf " ;"))]
             name))))

(deftype NoneOrMore [expr]
  EndoFunctor
  (fmap [_ f] (NoneOrMore. (fmap expr f)))

  GenEBNF
  (gen-ebnf [_]
    (EBNF. (for [expr-ebnf (.x (evaluate expr pure-ebnf gen-ebnf))]
             (str "{ " expr-ebnf " }")))))

(deftype Opt [expr]
  EndoFunctor
  (fmap [_ f] (Opt. (fmap expr f)))

  GenEBNF
  (gen-ebnf [_]
    (EBNF. (for [expr-ebnf (.x (evaluate expr pure-ebnf gen-ebnf))]
             (str "[ " expr-ebnf " ]")))))

(defmacro defrule [sym rule]
  `(let [~sym (effects.free/free (->Rule '~sym (term "")))]
     (def ~sym (effects.free/free (->Rule '~sym ~rule)))))

(defn all [& parsers]
  (fapply* (pure concat) parsers))

(def one-of (comp free-plus list))

(def optional (comp free ->Opt))

(def none-or-more (comp free ->NoneOrMore))

(defn one-or-more [parser]
  (all parser (none-or-more parser)))




(defn ebnf [rule]
  (second (extract ((.x (evaluate rule pure-ebnf gen-ebnf)) {}))))

(defn print-ebnf [rule]
  (doseq [[name rule] (reverse (ebnf rule))]
    (println name ":=" rule)))

(defrule upper-case (apply one-of (map term "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
(defrule lower-case (apply one-of (map term "abcdefghijklmnopqrstuvwxyz")))
(defrule letter (plus upper-case lower-case))
(defrule integer (all (optional (one-of (term '+) (term '-)))
                      (apply one-of (map term "123456789"))
                      (none-or-more (apply one-of (map term "0123456789")))))
(defrule whitespace (apply one-of (map term " \n\t\f")))
(defrule word (one-or-more letter))
(defrule form (one-of integer
                      word
                      (all (term \()
                           (none-or-more form)
                           (none-or-more whitespace)
                           (term \)))))

(println)
(print-ebnf form)
