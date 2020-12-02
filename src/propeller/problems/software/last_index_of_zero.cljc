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
   [propeller.tools.math :as math]))

; TODO We should pull out the repetition of `random-int`
; into someplace like `util`.
; Random integer between -50 and 50
(defn random-int [] (- (rand-int 51) 50))

(def instructions
  (utils/not-lazy
   (concat
    ;; stack-specific instructions
    (get-stack-instructions #{:integer :boolean :vector_integer})
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

(def train-and-test-data
  (let [edn-data (rest (utils/load-edn "data/last-index-of-zero/last-index-of-zero-edge.edn"))]
    {:train {:inputs (map (comp vector first) edn-data)
             :outputs (map second edn-data)}
     ; TODO – This should be a non-trivial subset of the random cases.
     ; Nic thinks that Tom Helmuth used 1,000 cases in his dissertation work.
     :test  []}))

; Helper function for error function
(defn last-index-of-zero-test-cases
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