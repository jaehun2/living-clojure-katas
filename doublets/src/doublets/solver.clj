(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.zip :as zip]
            [clojure.string :as string]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn print-tree [loc]
  (when-not (zip/end? loc)
    (do
      (println (str (string/join "" (repeat (count (zip/path loc)) " "))
                    (if (zip/branch? loc)
                      (first (zip/node loc))
                      (zip/node loc))))
      (recur (zip/next loc)))))

(defn my-make-node [n children]
  (if (not (coll? n))
    (vector n)
    (vec (cons (first n) children))))

(defn my-zipper [v]
  (zip/zipper coll? next my-make-node v))

(defn history [loc]
  (loop [loc loc
         result (list)]
    (if (nil? loc)
      (vec result)
      (recur (zip/up loc) (cons (first (zip/node loc)) result)))))

(defn find-candidates [word history]
  (filter (fn [w]
            (let [word-length (count word)
                  count-diff (fn [w1 w2]
                               (count (filter #(let [[x y] %] (not= x y)) (map vector w1 w2))))]
              (and (= word-length (count w))
                   (= 1 (count-diff w word))
                   (= -1 (.indexOf history w)))))
          words))

(defn doublets [word1 word2]
  (let [append-children (fn [loc]
                          (if (zip/branch? loc)
                            (let [current-word (first (zip/node loc))
                                  words (find-candidates current-word (history loc))]
                              (loop [loc loc
                                     words words]
                                (let [word (first words)]
                                  (cond
                                    (nil? word) loc
                                    (= word word2) (recur (zip/append-child loc word) (next words))
                                    :else (recur (zip/append-child loc [word]) (next words))))))
                            loc))
        doublets-tree (loop [loc (my-zipper [word1])]
                        (if (zip/end? loc)
                          (zip/root loc)
                          (recur (zip/next (append-children loc)))))]
    (loop [loc (my-zipper doublets-tree)]
      (if (zip/end? loc)
        []
        (if (= word2 (zip/node loc))
          (conj (vec (butlast (history loc))) word2)
          (recur (zip/next loc)))))))
