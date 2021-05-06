;; checksum.cljc
;; Erik Rauer and Nic McPhee,
;; based heavily on the Clojush implementation by 
;; Tom Helmuth, thelmuth@cs.umass.edu
;; https://github.com/lspector/Clojush/blob/master/src/clojush/problems/software/checksum.clj
;;
;; Problem Source:
;;   C. Le Goues et al., "The ManyBugs and IntroClass Benchmarks for Automated Repair of C Programs,"
;;   in IEEE Transactions on Software Engineering, vol. 41, no. 12, pp. 1236-1256, Dec. 1 2015.
;;   doi: 10.1109/TSE.2015.2454513
;;
;; Given a string (max length 50), compute the integer values of the characters
;; in the string, sum them, take the sum modulo 64, add the value of the \space 
;; character, and then convert that integer back into its corresponding character
;; (the checksum). Program must print "Check sum is X", where X is replaced by
;; the correct checksum.
;;
;; input stack has the input string

(ns propeller.problems.software.checksum
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.push.utils.helpers :refer [get-stack-instructions]]
            [propeller.utils :as utils]
            [propeller.tools.metrics :as metrics]
            [propeller.tools.math :as math]
            #?(:cljs [cljs.reader :refer [read-string]])))

; TODO We should pull out the repetition of `random-int`
; into someplace like `util`.
; Random integer between -128 and 128
(defn random-int [] (- (rand-int 257) 128))

(def instructions
  (utils/not-lazy
    (concat
    ;; stack-specific instructions
    (get-stack-instructions #{:exec :integer :boolean :char :string :print})
    (repeat 20 'close)
    ;; input instructions
    (list :in1)
    (list \space "Check sum is ")
    ;; ERCs
    (list 64 random-int)
    (list (fn [] (rand-nth (concat [\newline \tab] (map char (range 32 127)))))))))

(def train-and-test-data
  (let [edn-edge-data (utils/load-edn "data/checksum/checksum-edge.edn")
        edn-random-data
        (take 2194 (shuffle
                    (utils/load-edn "data/checksum/checksum-random.edn")))
        train-data (concat edn-edge-data (take 194 edn-random-data))
        test-data (take-last 2000 edn-random-data)]
    {:train {:inputs (map (comp vector :input1) train-data)
             :outputs (map :output1 train-data)}
     :test {:inputs (map (comp vector :input1) test-data)
            :outputs (map :output1 test-data)}}))


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
                          (assoc state/empty-state
                                 :input {:in1 (first input)}
                                 :output '(""))
                          (:step-limit argmap))
                         :output))
                      inputs)
         errors (mapcat (fn [correct-output output]
                       (vector
                        (metrics/levenshtein-distance correct-output output)
                        (if (not (empty? output))
                          (math/abs (- (int (last correct-output)) (int (last output)))) ;distance from correct last character
                          1000) ;penalty for wrong format
                        ))
                     correct-outputs
                     outputs)]
     (assoc individual
            :behaviors outputs
            :errors errors
            :total-error #?(:clj (apply +' errors)
                            :cljs (apply + errors))))))
