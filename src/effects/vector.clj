; Copyright (c) Jim Duey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this
; distribution. By using this software in any fashion, you are
; agreeing to be bound by the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns effects.vector
  (:refer-clojure :exclude [extend for])
  (:require [effects :refer :all]
            [effects.id :refer [id]]))

(extend-type clojure.lang.PersistentVector
  Applicative
  (wrap [_ v]
    [v])

  Monad
  (flat-map [vs f]
    (vec (mapcat f vs)))

  MonadZero
  (zero [_] [])
  (plus* [mv mvs] (vec (apply concat mv mvs))))
