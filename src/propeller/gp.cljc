(ns propeller.gp
  (:require [clojure.string]
            [propeller.genome :as genome]
            [propeller.variation :as variation]
            [propeller.quick-check :as quick-check]
            [propeller.push.instructions.bool]
            [propeller.push.instructions.character]
            [propeller.push.instructions.code]
            [propeller.push.instructions.input-output]
            [propeller.push.instructions.numeric]
            [propeller.push.instructions.polymorphic]
            [propeller.push.instructions.string]
            [propeller.push.instructions.vector]))

(defn report
  "Reports information each generation."
  [pop generation argmap]
  (let [best (first pop)]
    (println "-------------------------------------------------------")
    (println "               Report for Generation" generation)
    (println "-------------------------------------------------------")
    (print "Best plushy: ") (prn (:plushy best))
    (print "Best program: ") (prn (genome/plushy->push (:plushy best) argmap))
    (println "Best total error:" (:total-error best))
    (println "Best errors:" (:errors best))
    (println "Best behaviors:" (:behaviors best))
    (println "Genotypic diversity:"
             (float (/ (count (distinct (map :plushy pop))) (count pop))))
    (println "Behavioral diversity:"
             (float (/ (count (distinct (map :behaviors pop))) (count pop))))
    (println "Average genome length:"
             (float (/ (reduce + (map count (map :plushy pop))) (count pop))))
    (println "Average total error:"
             (float (/ (reduce + (map :total-error pop)) (count pop))))
    (println)))

(defn end-run
  "Run on Success"
  [generation best-individual
   {:keys [error-function] :as argmap}]
  ;; Failed to generate new training case; verify on testing cases
  (do (println "SUCCESS at generation" generation)
      (print "Checking program on test cases... ")
      (if (zero? (:total-error (error-function argmap best-individual :test)))
        (println "Test cases passed.")
        (println "Test cases failed."))
      (#?(:clj shutdown-agents))))

(defn gp
  "Main GP loop."
  [{:keys [population-size max-generations error-function instructions
           max-initial-plushy-size
           use-quick-check]
    :as   argmap}]
  ;;
  (println "Starting GP with args: " argmap)
  ;;
  (loop [generation 0
         population (repeatedly
                     population-size
                     #(hash-map :plushy (genome/make-random-plushy
                                         instructions
                                         max-initial-plushy-size)))
         ;; We probably want this to be a function of argmap so that
         ;; it can build the initial test cases in a dynamic way based
         ;; on the problem info in the argmap.
         qc-args (quick-check/make-gp-loop-args argmap)]
    (let [evaluated-pop (sort-by :total-error
                                 (#?(:clj  pmap
                                     :cljs map)
                                  (if use-quick-check
                                    #(error-function argmap % (:training-cases qc-args))
                                    (partial error-function argmap))
                                  population))
          best-individual (first evaluated-pop)]
      (report evaluated-pop generation argmap)
      (cond
        (and (not use-quick-check) (zero? (:total-error best-individual)))
        (end-run generation best-individual argmap)
        ;;
        (>= generation max-generations)
        nil
        ;;
        (and use-quick-check (quick-check/add-new-training-case? best-individual qc-args argmap))
        (let [new-training-case (quick-check/make-new-training-case best-individual qc-args argmap)]
          (if (nil? new-training-case)
            (end-run generation best-individual argmap)
            ;; add new test case and recur
            ;; ...
            (recur (inc generation)
                   (if (:elitism argmap)
                     (conj (repeatedly (dec population-size)
                                       #(variation/new-individual evaluated-pop argmap))
                           (first evaluated-pop))
                     (repeatedly population-size
                                 #(variation/new-individual evaluated-pop argmap)))
                   ;; This is kind of messy and could use some cleaning up
                   (update-in
                    (update-in (assoc qc-args :gens-since-training-add 0)
                               [:training-cases :inputs]
                               #(cons new-training-case %))
                    [:training-cases :outputs]
                    (let [target-function (:target-function argmap)]
                      #(cons (target-function new-training-case) %))))))
        ;;
        :else (recur (inc generation)
                     (if (:elitism argmap)
                       (conj (repeatedly (dec population-size)
                                         #(variation/new-individual evaluated-pop argmap))
                             (first evaluated-pop))
                       (repeatedly population-size
                                   #(variation/new-individual evaluated-pop argmap)))
                     (update qc-args :gens-since-training-add inc))))))
