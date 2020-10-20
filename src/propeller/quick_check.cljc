(ns propeller.quick-check
  (:require [clojure.string]
            [propeller.genome :as genome]))

(def arg-defaults 
  {
   :use-quick-check            false
   :num-initial-training-cases 5
  })

(defn make-gp-loop-args [argmap]
  (let [num-initial (:num-initial-training-cases argmap)]
    {:training-cases {:inputs  (take num-initial (get-in argmap [:train-and-test-data :train :input]))
                      :outputs (take num-initial (get-in argmap [:train-and-test-data :train :output]))}}))

(defn add-new-training-case?
  [best-individual]
  (zero? (:total-error best-individual)))

(defn make-new-training-case [] nil)