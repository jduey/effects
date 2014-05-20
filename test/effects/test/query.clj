(ns effects.test.query
  (:refer-clojure :exclude [for seq extend])
  (:require [effects :refer :all]
            [effects.free :refer :all])
  (:import [effects.free Pure Free FreeA]))

(deftype Select [fields])
(deftype Table [table-name alias])
(deftype From [sources])
(deftype Where [expression])
(deftype Join [])
(deftype GroupBy [])
(deftype Having [])
(deftype OrderBy [])

(deftype Equals [x y])
(deftype NotEquals [x y])
(deftype GreaterThan [x y])
(deftype LessThan [x y])
(deftype GreaterThanOrEquals [x y])
(deftype LessThanOrEquals [x y])
(deftype Between [v low high])
(deftype In [v vals])
(deftype And [exprs])
(deftype Or [exprs])
(deftype Not [expr])

#_(sql
 (select f1 f2)
 (from (join t1 t2))
 (where (and (= f1 "bogus")
             (> f2 8)))
 )

(deftype Find [lvars])
(deftype In [sources])
(deftype Where [datom-forms])

#_(datomic
 )
