(ns tiny-maze.solver
  (:refer-clojure :exclude [* - + == /])
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer :all]))

(def maze [[:S 0 1]
           [1 0 1]
           [1 0 :E]])

(def maze2 [[:S 0 0 1]
            [1 1 0 0]
            [1 0 0 1]
            [1 1 0 :E]])

(def maze3 [[:S 0 0 1 0]
            [1  1 0 0 0]
            [1  0 0 1 1]
            [1  1 0 0 1]
            [1  0 0 0 :E]])

(defn solve-maze [maze]
  (let [width (count (first maze))
        height (count maze)
        blocked? (fn [pos] (or (= pos :x) (= pos 1)))
        north? (fn [r c current-path]
                 (cond
                   (= r 0) 1
                   (blocked? (mget current-path (dec r) c)) 1
                   :else 0))
        south? (fn [r c current-path]
                 (cond
                   (= r (dec height)) 1
                   (blocked? (mget current-path (inc r) c)) 1
                   :else 0))
        east? (fn [r c current-path]
                (cond
                  (= c (dec width)) 1
                  (blocked? (mget current-path r (inc c))) 1
                  :else 0))
        west? (fn [r c current-path]
                (cond
                  (= c 0) 1
                  (blocked? (mget current-path r (dec c))) 1
                  :else 0))
        available-dirs (fn [[r c] current-path]
                         [(north? r c current-path)
                          (south? r c current-path)
                          (east? r c current-path)
                          (west? r c current-path)])
        dir? (fn [dirs]
               (condp = (.indexOf dirs 0)
                 0 :north
                 1 :south
                 2 :east
                 3 :west))
        next-path-and-pos (fn [dir current-path [r c]]
                            (condp = dir
                              :north [(mset current-path r c :x) [(dec r) c]]
                              :south [(mset current-path r c :x) [(inc r) c]]
                              :east [(mset current-path r c :x) [r (inc c)]]
                              :west [(mset current-path r c :x) [r (dec c)]]))
        start-pos [0 0]]
    (loop [current-path maze
           current-pos start-pos
           dirs (available-dirs start-pos current-path)
           saved-infos []]
      (let [[r c] current-pos]
        (if (= (mget current-path r c) :E)
          (mset current-path r c :x)
          (condp = (count (filter #(= 0 %) dirs))
            0 (if (= (count saved-infos) 0)
                nil
                (let [[saved-path saved-pos dirs dir-taken] (peek saved-infos)
                      updated-dirs (condp = dir-taken
                                     :north (assoc dirs 0 1)
                                     :south (assoc dirs 1 1)
                                     :east (assoc dirs 2 1)
                                     :west (assoc dirs 3 1))]
                  (recur saved-path saved-pos updated-dirs (pop saved-infos))))
            1 (let [dir (dir? dirs)
                    [next-path next-pos] (next-path-and-pos dir current-path current-pos)
                    next-dirs (available-dirs next-pos next-path)]
                (recur next-path next-pos next-dirs saved-infos))
            (let [dir (dir? dirs)
                  [next-path next-pos] (next-path-and-pos dir current-path current-pos)
                  next-dirs (available-dirs next-pos next-path)
                  updated-saved-infos (conj saved-infos [next-path next-pos next-dirs dir])]
              (recur next-path next-pos next-dirs updated-saved-infos))))))))

(solve-maze maze)
(solve-maze maze2)
(solve-maze maze3)