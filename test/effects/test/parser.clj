(ns effects.test.parser
  (:refer-clojure :exclude [for seq extend])
  (:require [effects :refer :all]
            [effects.free :refer :all]
            [effects.state :refer :all])
  (:import [effects.free Pure Free FreeA FreePlus FreeZero]))

(defn term [pred]
  (free pred))

(defn all [& parsers]
  (fapply* (pure list) parsers))

(def one-of plus)

(defn optional [parser]
  (plus parser (zero parser)))

(deftype OneOrMore [parser meta]
  Object
  (toString [_]
    (pr-str parser))

  clojure.lang.IObj
  (withMeta [_ m] (OneOrMore. parser m))

  clojure.lang.IMeta
  (meta [_] meta)

  Monoid
  (zero [_] free-zero)
  (plus* [p ps]
    (free-plus (cons p ps))))

(defn one-or-more [parser]
  (OneOrMore. parser nil))

(defn none-or-more [parser]
  (optional (one-or-more parser)))

(deftype TermTester [v]
  Object
  (toString [_]
    (condp = v
      \newline "\"\\n\""
      \tab "\"\\t\""
      \space "\" \""
      \formfeed "\"\\f\""
      (format "\"%s\"" v)))

  clojure.lang.IFn
  (invoke [_ x]
    (= x v)))

(defmethod print-method TermTester [pred w]
  (.write w (str (.v pred))))

(defn is-term [v]
  (term (TermTester. v)))

(defmacro defrule [sym rule]
  `(let [~sym (->Free '~sym {})]
     (def ~sym (with-meta ~rule {:rule-name (name '~sym)}))))

(defrule integer (one-or-more (apply one-of (map is-term "0123456789"))))
(defrule upper-case (apply one-of (map is-term "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
(defrule lower-case (apply one-of (map is-term "abcdefghijklmnopqrstuvwxyz")))
(defrule letter (plus upper-case lower-case))
(defrule whitespace (apply one-of (map is-term " \n\t\f")))
(defrule word (one-or-more letter))
(defrule form (one-of integer
                      word
                      (all (is-term \()
                           (none-or-more form)
                           (none-or-more whitespace)
                           (is-term \)))))

(defprotocol EBNF
  (gen-ebnf [rule]))

(extend-type Free
  EBNF
  (gen-ebnf [rule]
    (state (str (.v rule)))))

(extend-type FreeZero
  EBNF
  (gen-ebnf [rule]
    (state "")))

#_(extend-type FreeA
  EBNF
  (gen-ebnf [rule]
    (let [rule-name (get (meta rule) :rule-name)]
      (if (nil? rule-name)
        (fapply* (state (fn [& args]
                          (apply str (interpose " , " args))))
                 (map gen-ebnf (.args rule)))
        (for [found-name (get-val rule-name ::not-found)
              rule-str (if (= ::not-found found-name)
                         (for [_ (set-val rule-name "")
                               rule-str (fapply* (state (fn [& args]
                                                          (apply str (interpose " , " args))))
                                                 (map gen-ebnf (.args rule)))
                               _ (set-val rule-name (str rule-str " ;"))]
                           rule-name)
                         (state rule-name))]
          rule-str)))))

(extend-type FreeA
  EBNF
  (gen-ebnf [rule]
    (fmap rule (constantly (state :bogus)))))

(defn gen-ebnf-alts [rule]
  (fapply* (state (fn [& args]
                    (if (and (= 2 (count args))
                             (= (second args) ""))
                      (format "[ %s ]" (first args))
                      (apply str (interpose " | " args)))))
           (map gen-ebnf (.alts rule))))

(extend-type FreePlus
  EBNF
  (gen-ebnf [rule]
    (let [rule-name (get (meta rule) :rule-name)]
      (if (nil? rule-name)
        (gen-ebnf-alts rule)
        (for [found-name (get-val rule-name ::not-found)
              rule-str (if (= ::not-found found-name)
                         (for [_ (set-val rule-name "")
                               rule-str (gen-ebnf-alts rule)
                               _ (set-val rule-name (str rule-str " ;"))]
                           rule-name)
                         (state rule-name))]
          rule-str)))))

(extend-type OneOrMore
  EBNF
  (gen-ebnf [rule]
    (let [rule-name (get (meta rule) :rule-name)]
      (if (nil? rule-name)
        (gen-ebnf (.parser rule))
        (for [found-name (get-val rule-name ::not-found)
              rule-str (if (= ::not-found found-name)
                         (for [_ (set-val rule-name "")
                               rule-str (gen-ebnf (.parser rule))
                               _ (set-val rule-name
                                          (format "{ %s } ;" rule-str))]
                           rule-name)
                         (state rule-name))]
          rule-str)))))

#_(extend-type clojure.lang.Symbol
  EBNF
  (gen-ebnf [rule]
    (state (name rule))))

(defn ebnf [parser]
  (second ((gen-ebnf parser) {})))

#_(doall (map (fn [[k v]] (println k " := " v)) (ebnf form)))
(println ((gen-ebnf (all (is-term "a") (is-term "b")))))
