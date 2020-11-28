(ns propeller.problems.software.smallest
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.push.utils.helpers :refer [get-stack-instructions]]
            [propeller.utils :as utils]
            [clojure.test.check.generators :as gen]
            #?(:cljs [cljs.reader :refer [read-string]])))

;; =============================================================================
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; SMALLEST PROBLEM
;;
;; This problem file defines the following problem:
;; There are four ints as inputs. The program must read them in, 
;; find the smallest one, and print the result.
;;
;; Problem Source: C. Le Goues et al., "The ManyBugs and IntroClass Benchmarks
;; for Automated Repair of C Programs," in IEEE Transactions on Software
;; Engineering, vol. 41, no. 12, pp. 1236-1256, Dec. 1 2015.
;; doi: 10.1109/TSE.2015.2454513
;;
;; NOTE: input stack: in1 (int),
;;                    in2 (int),
;;                    in3 (int),
;;                    in4 (int),
;;       output stack: printed output
;; =============================================================================

;; =============================================================================
;; DATA DOMAINS
;;
;; A list of data domains. Each domain is a map containing a "set" of inputs
;; and two integers representing how many cases from the set should be used as
;; training and testing cases respectively. Each "set" of inputs is either a
;; list or a function that, when called, will create a random element of the set
;; =============================================================================

; Random integer between -100 and 100
(defn random-int [] (- (rand-int 201) 100))

(def instructions
  (utils/not-lazy
    (concat
      ;; stack-specific instructions
      (get-stack-instructions #{:boolean :exec :integer :print})
      ;; input instructions
      (list :in1 :in2 :in3 :in4)
      ;; ERCs (constants)
      (list random-int))))

(defn target-function
  [[i1 i2 i3 i4]]
  (min i1 i2 i3 i4))

(def train-and-test-data
  (let [inputs (vec (repeatedly 1100 #(vector (random-int) (random-int)
                                              (random-int) (random-int))))
        outputs (mapv target-function inputs)
        train-set {:inputs  (take 100 inputs)
                   :outputs (take 100 outputs)}
        test-set {:inputs  (drop 100 inputs)
                  :outputs (drop 100 outputs)}]
    {:train train-set
     :test  test-set}))

(defn make-initial-state
  [input]
  (assoc state/empty-state :input {:in1 (get input 0)
                                   :in2 (get input 1)
                                   :in3 (get input 2)
                                   :in4 (get input 3)}
         :output '("")))

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
                            (make-initial-state input)
                            (:step-limit argmap))
                          :output))
                      inputs)
         errors (map (fn [correct-output output]
                       (let [parsed-output (try (read-string output)
                                                #?(:clj (catch Exception e 1000.0)
                                                   :cljs (catch js/Error. e 1000.0)))]
                         (if (= correct-output parsed-output) 0 1)))
                     correct-outputs
                     outputs)]
     (assoc individual
       :behaviors outputs
       :errors errors
       :total-error #?(:clj (apply +' errors)
                       :cljs (apply + errors))))))

(def test-case-generator
  (gen/vector (gen/choose -100 100) 4))

(def argmap {:instructions        instructions
             :target-function     target-function
             :error-function      error-function
             :train-and-test-data train-and-test-data
             :test-case-generator test-case-generator
             :make-initial-state  make-initial-state
             :result-stack        :output})