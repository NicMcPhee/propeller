(ns propeller.quick-check
  (:require [clojure.string]
            [propeller.genome :as genome]))

(def arg-defaults 
  {:use-quick-check            false
   :num-initial-training-cases 5
   :average-error-threshold    0
   :zero-error-threshold       1.0
   :gens-to-add-new-training   20})

(defn make-gp-loop-args [argmap]
  (let [num-initial (:num-initial-training-cases argmap)]
    {:training-cases {:inputs  (take num-initial (get-in argmap [:train-and-test-data :train :inputs]))
                      :outputs (take num-initial (get-in argmap [:train-and-test-data :train :outputs]))}
     :gens-since-training-add 0}))

(defn num-training-cases [gp-loop-args]
  (count (get-in gp-loop-args [:training-cases :inputs])))

; We've looked at:
; * Add test case every K generations
; * Add test case if average total error below threshold
; * Add test case if proportion of number of zero errors above threshold 
(defn add-new-training-case?
  [argmap gp-loop-args best-individual]
  (or
   (<= (:total-error best-individual) (* (num-training-cases gp-loop-args) 
                                         (:average-error-threshold argmap)))
   (>= (count (filter zero? (:errors best-individual))) 
       (* (num-training-cases gp-loop-args)
          (:zero-error-threshold argmap)))
   (>= (:gens-since-training-add gp-loop-args)
       (:gens-to-add-new-training argmap))
   ))

(defn make-new-training-case [] nil)
