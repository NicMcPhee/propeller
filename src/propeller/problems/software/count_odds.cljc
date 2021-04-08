(ns propeller.problems.software.count-odds
  (:require
   [propeller.push.state :as state]
   [propeller.push.interpreter :as interpreter]
   [propeller.genome :as genome]
   [propeller.push.utils.helpers :refer [get-stack-instructions]]
   [propeller.utils :as utils]
   [propeller.tools.math :as math]))

; TODO We should pull out the repetition of `random-int`
; into someplace like `util`.
; Random integer between -50 and 50
(defn random-int [] (- (rand-int 51) 50))

(def instructions
  (utils/not-lazy
   (concat
    ;; stack-specific instructions
    (get-stack-instructions #{:integer :boolean :vector_integer :exec})
    (repeat 20 'close)
    ;; input instructions
    (list :in1)
    ; Tom's original benchmark paper says to include 0, 1, 2, and random integers.
    (list 0 1 2 random-int))))

; Tom's original benchmark paper says to have 200 training cases and 2,000
; test/validation cases.
; Inputs should be vectors whose length is in the range [0, 50]
; with each integer being in the range [-1000, 1000].
(def train-and-test-data
  (let [edn-edge-data (rest (utils/load-edn "data/count-odds/count-odds-edge.edn"))
        edn-random-data (take 1078 (rest (utils/load-edn "data/count-odds/count-odds-random.edn")))
        train-data (concat edn-edge-data (take 78 edn-random-data))
        test-data (take-last 1000 edn-random-data)]
    {:train {:inputs (map (comp vector first) train-data)
             :outputs (map second train-data)}
     ; TODO – This should be a non-trivial subset of the random cases.
     ; Nic thinks that Tom Helmuth used 1,000 cases in his dissertation work.
     :test {:inputs (map (comp vector first) test-data)
            :outputs (map second test-data)}}))

; Helper function for error function
(defn count-odds-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map #(vector (first %) (.lastIndexOf (first %) 0))
       inputs))

(defn error-function
  ([argmap individual]
   (error-function argmap individual :train))
  ([argmap individual subset]
   (let [program (genome/plushy->push (:plushy individual))
         data (get train-and-test-data subset)
         inputs (:inputs data)
         correct-outputs (:outputs data)
         outputs (map (fn [input]
                        (state/peek-stack
                         (interpreter/interpret-program
                          program
                          (assoc state/empty-state :input {:in1 (get input 0)})
                          (:step-limit argmap))
                         :integer))
                      inputs)
         errors (map (fn [correct-output output]
                      (if (= output :no-stack-item)
                        1000000
                        (math/abs (- correct-output output))))
                     correct-outputs
                     outputs)]
     (assoc individual
            :behaviors outputs
            :errors errors
            :total-error #?(:clj (apply +' errors)
                            :cljs (apply + errors))))))
