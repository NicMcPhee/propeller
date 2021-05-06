;; last_index_of_zero.clj
;; Nic McPhee, mcphee@morris.umn.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a vector of integers of length <= 50, each integer in the range [-50,50],
;; at least one of which is 0, return the index of the last occurance of 0 in the vector.
;;
;; input stack has 1 input vector of integers

(ns propeller.problems.software.last-index-of-zero
  (:require
   [propeller.push.state :as state]
   [propeller.push.interpreter :as interpreter]
   [propeller.genome :as genome]
   [propeller.push.utils.helpers :refer [get-stack-instructions]]
   [propeller.utils :as utils]
   [propeller.tools.math :as math]
   [clojure.test.check.generators :as gen]))

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
    ;; ERCs
    ;; Clojush implementation actually provides random integers 
    ;; in the range [-50, 50], but the paper says to use only zero.
    ;; I talked to Tom Helmuth, and it's clearly just an "oops"
    ;; that these aren't the same. We should probably use the
    ;; Clojush version for better comparisons, but we might also
    ;; want to include 0 and see what impact that has. (It seems
    ;; likely to make the problem easier.)
    ;; (list 0 random-int)
    (list random-int))))

(defn target-function
  [[i1]]
  (.lastIndexOf i1 0))

(def train-and-test-data
  (let [edn-edge-data (utils/load-edn "data/last-index-of-zero/last-index-of-zero-edge.edn")
        edn-random-data 
        (take 1078 (shuffle
                    (utils/load-edn "data/last-index-of-zero/last-index-of-zero-random.edn")))
        train-data (concat edn-edge-data (take 78 edn-random-data))
        test-data (take-last 1000 edn-random-data)]
    {:train {:inputs (map (comp vector :input1) train-data)
             :outputs (map :output1 train-data)}
     ; TODO – This should be a non-trivial subset of the random cases.
     ; Nic thinks that Tom Helmuth used 1,000 cases in his dissertation work.
     :test {:inputs (map (comp vector :input1) test-data)
            :outputs (map :output1 test-data)}}))

; Helper function for error function
(defn last-index-of-zero-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map #(vector (first %) (.lastIndexOf (first %) 0))
       inputs))

(defn error-function
  ([argmap individual]
   (error-function argmap individual (:train train-and-test-data)))
  ([argmap individual data]
   (let [program (genome/plushy->push (:plushy individual))
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
                            :cljs (apply + errors))
            :num-zero-errors (count (filter zero? errors))))))

(defn at-least-one-zero
  [xs]
  (if (>= (.indexOf xs 0) 0)
    xs
    ; Pick a random element and set it to zero.
    (let [n (rand-int (count xs))]
      (assoc xs n 0))))

(def test-case-generator
  (gen/vector
  (gen/fmap at-least-one-zero (gen/vector (gen/choose -50 50) 1 50))
   1))

(defn make-initial-state
  [input]
  (assoc state/empty-state :input {:in1 (get input 0)}))

(def argmap {:instructions        instructions
             :target-function     target-function
             :error-function      error-function
             :train-and-test-data train-and-test-data
             :test-case-generator test-case-generator
             :make-initial-state  make-initial-state
             :result-stack        :integer})
