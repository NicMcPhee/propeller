(ns propeller.core
  #?(:clj (:gen-class))
  (:require [propeller.gp :as gp]
            [propeller.quick-check :as quick-check]
            [propeller.problems.simple-regression :as regression]
            [propeller.problems.string-classification :as string-classif]
            #?(:cljs [cljs.reader :refer [read-string]])))

(defn eval-problem-var
  [problem-name var-name]
  (eval (symbol (str "propeller.problems." problem-name "/" var-name))))

(defn -main
  "Runs propel-gp, giving it a map of arguments."
  [& args]
  (when (empty? args)
    (println "You must specify a problem to run.")
    (println "Try, for example:")
    (println "   lein run software.smallest")
    (System/exit 1))
  (require (symbol (str "propeller.problems." (first args))))
  (gp/gp
    (update-in
      (merge
        {:max-generations         500
         :population-size         500
         :max-initial-plushy-size 100
         :step-limit              200
         :parent-selection        :lexicase
         :tournament-size         5
         :umad-rate               0.1
         :variation               {:umad 0.5 :crossover 0.5}
         :elitism                 false
         :sort-by-total-error     true}
        quick-check/arg-defaults
        ;number-io/argmap
        (require (symbol (str "propeller.problems." (first args))))
        (eval (symbol (str "propeller.problems." (first args) "/argmap")))
        (apply hash-map
               (map read-string (rest args))))
      [:error-function]
      identity)))
