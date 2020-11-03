(ns propeller.quick-check
  (:require [clojure.string]
            [propeller.push.state :as state]
            [propeller.push.interpreter :as interpreter]
            [propeller.genome :as genome]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

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
  [best-individual gp-loop-args argmap]
  (or
   (<= (:total-error best-individual) (* (num-training-cases gp-loop-args) 
                                         (:average-error-threshold argmap)))
   (>= (count (filter zero? (:errors best-individual))) 
       (* (num-training-cases gp-loop-args)
          (:zero-error-threshold argmap)))
   (>= (:gens-since-training-add gp-loop-args)
       (:gens-to-add-new-training argmap))
   ))

(defn solves-target-function
  [target-function inputs program argmap]
  (prop/for-all [i (gen/such-that #(not (contains? inputs %)) (:test-case-generator argmap) 100)]
                (let [result-state (interpreter/interpret-program
                                    program
                                    ((:make-initial-state argmap) i)
                                    (:step-limit argmap))
                      result (state/peek-stack result-state :integer)]
                  (println "Trying new input")
                  (println i)
                  (println (sort inputs))
                  (println (contains? inputs i))
                  ; (println result-state)
                  ; (println result)
                  ; (println (target-function-hard i))
                  (and (not= result :no-stack-item)
                       (= (target-function i)
                          result)))))

(defn make-new-training-case 
  [winning-individual qc-args argmap] 
  (let [inputs (get-in qc-args [:training-cases :inputs])
        quick-check-result (tc/quick-check
                            100
                            (solves-target-function 
                             (:target-function argmap) 
                             inputs
                             ; Seems wasteful to have to redo this; might want to
                             ; add the push program to the individual's map.
                             (genome/plushy->push (:plushy winning-individual))
                             argmap))]
    (println quick-check-result)
    (cond (:pass? quick-check-result) nil
          ; (contains? inputs (first (:smallest (:shrunk quick-check-result)))) (first (:fail quick-check-result))
          :else (first (:smallest (:shrunk quick-check-result))))))
