(ns propeller.utils
  (:require
   [clojure.java.io :as io]
   [clojure.edn :as edn]))

(defn indexof
  "Returns the first index of an element in a collection. If the element is not
  present in the collection, returns -1."
  [s v]
  (loop [idx 0 items s]
    (cond
      (empty? items) -1
      (= v (first items)) idx
      :else (recur (inc idx) (rest items)))))

(defn not-lazy
  "Returns lst if it is not a seq, or a non-lazy version of lst if it is."
  [lst]
  (if (seq? lst)
    (apply list lst)
    lst))

(defn ensure-list
  "Returns a non-lazy list if passed a seq argument. Othwrwise, returns a list
  containing the argument."
  [thing]
  (if (seq? thing)
    (not-lazy thing)
    (list thing)))

(defn random-instruction
  "Returns a random instruction from a supplied pool of instructions, evaluating
  ERC-producing functions to a constant literal."
  [instructions]
  (let [instruction (rand-nth instructions)]
    (if (fn? instruction)
      (instruction)
      instruction)))

(defn load-edn
  "Load edn from an io/reader source (filename or io/resource)."
  [source]
  (try
    (with-open [r (io/reader source)]
      (edn/read (java.io.PushbackReader. r)))

    (catch java.io.IOException e
      (printf "Couldn't open '%s': %s\n" source (.getMessage e)))
    (catch RuntimeException e
      (printf "Error parsing edn file '%s': %s\n" source (.getMessage e)))))
