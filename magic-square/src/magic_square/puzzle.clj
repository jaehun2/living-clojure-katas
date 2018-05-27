(ns magic-square.puzzle
  (:require [clojure.math.combinatorics :as combo]))

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn sum-rows [m]
  (map #(reduce + %) m))

(defn sum-cols [m]
  [(reduce + (map first m))
   (reduce + (map second m))
   (reduce + (map last m))])

(defn sum-diagonals [m]
  [(+ (get-in m [0 0]) (get-in m [1 1]) (get-in m [2 2]))
   (+ (get-in m [2 0]) (get-in m [1 1]) (get-in m [0 2]))])

(defn magic-square [values]
  (let [ps (combo/permutations values)]
    (loop [ps ps]
      (let [p (first ps)
            m (vec (map vec (partition 3 p)))
            magic-number-set (set (sum-rows m))]
        (if (and (= 1 (count magic-number-set))
                 (= magic-number-set
                    (set (sum-cols m))
                    (set (sum-diagonals m))))
          m
          (recur (rest ps)))))))

(magic-square values)

(def v1 [[1.5 4.0 3.5] [5.0 3.0 1.0] [2.5 2.0 4.5]])
(sum-rows v1)
(sum-cols v1)
(sum-diagonals v1)