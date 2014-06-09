(ns effects.test.parser
  (:refer-clojure :exclude [for seq extend])
  (:require [effects :refer :all]
            [effects.free :refer :all]
            [effects.maybe :refer :all]
            [effects.state :refer :all :exclude [set-val update-state]])
  (:import [effects.free Pure Free FreeA FreePlus FreeZero]))

(def state-maybe (plus state maybe))

(defn update-state [f]
  (->State maybe
           (fn [s] (maybe (list s (f s))))
           (pr-str "update-state")))

(defn set-val [k v]
  (update-state #(assoc % k v)))

(deftype EBNF [x]
  Applicative
  (fapply* [_ args]
    (EBNF. (fapply* (state-maybe (fn [& args]
                                   (apply str (interpose " , " args))))
                    (map #(.x %) args))))

  Monoid
  (plus* [v vs]
    (EBNF. (fapply* (state-maybe (fn [& args]
                                   (apply str (interpose " | " args))))
                    (map #(.x %) (cons v vs))))))

(defn term [v]
  (pure v))

(defn pure-parser [term]
  (if (fn? term)
    (state-maybe term)
    (for [[next-term] (update-state rest)
          :when (= term next-term)]
      [term])))

(defn pure-ebnf [v]
  (EBNF. (state-maybe (condp = v
                        \newline "\"\\n\""
                        \tab "\"\\t\""
                        \space "\" \""
                        \formfeed "\"\\f\""
                        (format "\"%s\"" v)))))
(defprotocol GenParser
  (gen-parser [_]))

(defprotocol GenEBNF
  (gen-ebnf [_]))

(deftype Opt [expr]
  EndoFunctor
  (fmap [_ f] (Opt. (fmap expr f)))

  GenParser
  (gen-parser [_]
    (plus (evaluate expr pure-parser gen-parser)
          (state-maybe [])))

  GenEBNF
  (gen-ebnf [_]
    (EBNF. (for [expr-ebnf (.x (evaluate expr pure-ebnf gen-ebnf))]
             (str "[ " expr-ebnf " ]")))))

(defn repeat-parser [parser]
  (for [x parser
        xs (plus (repeat-parser parser)
                 (state-maybe []))]
    (concat x xs)))

(deftype NoneOrMore [expr]
  EndoFunctor
  (fmap [_ f] (OneOrMore. (fmap expr f)))

  GenParser
  (gen-parser [_]
    (plus (repeat-parser (evaluate expr pure-parser gen-parser))
          (state-maybe [])))

  GenEBNF
  (gen-ebnf [_]
    (EBNF. (for [expr-ebnf (.x (evaluate expr pure-ebnf gen-ebnf))]
             (str "{ " expr-ebnf " }")))))

(deftype Rule [name expr handler-fn]
  EndoFunctor
  (fmap [_ f] (Rule. name (fmap expr f) handler-fn))

  GenParser
  (gen-parser [_]
    (if expr
      (for [parsed (evaluate expr pure-parser gen-parser)]
        (if handler-fn
          [(apply handler-fn parsed)]
          parsed))
      (for [parsed (evaluate (deref (resolve (symbol name)))
                             pure-parser gen-parser)]
        (if handler-fn
          [(apply handler-fn parsed)]
          parsed))))

  GenEBNF
  (gen-ebnf [_]
    (if expr
      (EBNF. (for [expr-ebnf (.x (evaluate expr pure-ebnf gen-ebnf))
                   _ (set-val name (str expr-ebnf " ;"))]
               name))
      (EBNF. (state-maybe name)))))

(deftype Ignore [expr]
  EndoFunctor
  (fmap [_ f] (Ignore. (fmap expr f)))

  GenParser
  (gen-parser [_]
    (for [_ (evaluate expr pure-parser gen-parser)]
      []))

  GenEBNF
  (gen-ebnf [_]
    (evaluate expr pure-ebnf gen-ebnf)))

(defmacro defrule
  ([sym]
     `(def ~sym (effects.free/free (->Rule '~sym nil nil))))
  ([sym rule]
     `(let [~sym (effects.free/free (->Rule '~sym nil nil))]
        (def ~sym (effects.free/free (->Rule '~sym ~rule nil)))))
  ([sym rule handler-fn]
     `(let [~sym (effects.free/free (->Rule '~sym nil nil))]
        (def ~sym (effects.free/free (->Rule '~sym ~rule ~handler-fn))))))

(defn all [& parsers]
  (fapply* (pure concat) parsers))

(def one-of (comp free-plus list))

(def optional (comp free ->Opt))

(defn none-or-more [parser]
  (free (->NoneOrMore parser)))

(defn one-or-more [parser]
  (all parser (none-or-more parser)))

(def ignore (comp free ->Ignore))

(defn parser [expr]
  (evaluate expr pure-parser gen-parser))




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
                      (none-or-more (apply one-of (map term "0123456789"))))
  (fn [& digits]
    (read-string (apply str digits))))

(defrule whitespace (apply one-of (map term " \n\t\f")))

(defrule word
  (one-or-more letter)
  str)

(defrule form)
(defrule s-expr (all (ignore (term \())
                     (none-or-more
                      (all (ignore (none-or-more whitespace))
                           (one-or-more form)))
                     (ignore (term \))))
  list)

(defrule form
  (one-of integer
          word
          s-expr))

(println)
(print-ebnf form)

(println)
(prn (extract ((parser form) "(str (add 15 92))")))
