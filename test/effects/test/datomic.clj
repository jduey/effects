
(def tbl (->>  [['sally :age 21]
                ['fred :age 42]
                ['ethel :age 42]
                ['fred :likes 'pizza]
                ['sally :likes 'opera]
                ['ethel :likes 'sushi]]
               (vec)))

(defn query [data age]
  (for [[e a v] tbl
        :when (= a :age)
        :when (= v age)]
    e))

(prn :q (query tbl 42))

(prn :r (for [[e1 a1 v1] tbl
              :when (= a1 :age)
              :when (= v1 42)
              [e2 a2 v2] tbl
              :when (and (= a2 :likes))
              :when (= e1 e2)
              ]
          [e2 v2]))

(deftype Where [input entity attr value time next]
  e/EndoFunctor
  (fmap [_ f]
    (Where. input entity attr value time (f next))))

(defmacro where [& [entity attr value time :as args]]
  (cond
   (= (count args) 0) (throw (Exception. "Bogus"))
   (<= (count args) 4) `(apply ->Where '$ ~args)
   (<= (count args) 5) `(apply ->Where ~args)
   :else (throw (Exception. "Bogus"))))

(def q (for [(where ?e :age 42)
             (where ?e :likes ?l)]
         [?e ?l]))
