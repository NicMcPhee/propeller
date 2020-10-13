(ns propeller.quick-check
  (:require [clojure.string]
            [propeller.genome :as genome]))

(def arg-defaults 
  {
   :use-quick-check false
  })

(def gp-loop-args
  {})

(defn add-new-training-case?
  [best-individual]
  (zero? (:total-error best-individual)))

(defn make-new-training-case [] nil)