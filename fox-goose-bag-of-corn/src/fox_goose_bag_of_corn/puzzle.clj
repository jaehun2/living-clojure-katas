(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set]))

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(defn river-crossing-plan []
  (let [final-step (map set [[] [:boat] [:fox :corn :goose :you]])
        left-to-right 0
        right-to-left 1]
    (loop [plans start-pos
           direction left-to-right]
      (let [last-step (last plans)
            left (first last-step)
            middle (second last-step)
            right (last last-step)]
        (if (= (map set last-step) final-step)
          plans
          (if (not= (count middle) 1)
            (let [to-be-added (vec (disj (set middle) :boat))]
              (if (= direction left-to-right)
                (recur (conj plans [left [:boat] (vec (concat right to-be-added))])
                       right-to-left)
                (recur (conj plans [(vec (concat left to-be-added)) [:boat] right])
                       left-to-right)))
            (let [choose-from-left (fn [coll]
                                     (let [except-you (disj (set coll) :you)
                                           except-you-size (count except-you)]
                                       (cond
                                         (= 1 except-you-size) coll
                                         (and (= 2 except-you-size)
                                              (= except-you #{:fox :corn})) [:you :fox]
                                         (and (= 2 except-you-size)
                                              (= except-you #{:goose :corn})) [:you :corn]
                                         :else [:you :goose])))
                  choose-from-right (fn [coll]
                                      (let [except-you (disj (set coll) :you)
                                            except-you-size (count except-you)]
                                        (cond
                                          (= 1 except-you-size) [:you]
                                          (and (= 2 except-you-size)
                                               (= except-you #{:fox :corn})) [:you]
                                          (and (= 2 except-you-size)
                                               (= except-you #{:fox :goose})) [:you :goose])))]
              (if (= direction left-to-right)
                (let [to-be-loaded (choose-from-left left)]
                  (recur (conj plans [(vec (clojure.set/difference (set left) (set to-be-loaded)))
                                      (vec (concat middle to-be-loaded))
                                      right])
                         left-to-right))
                (let [to-be-loaded (choose-from-right right)]
                  (recur (conj plans [left
                                      (vec (concat middle to-be-loaded))
                                      (vec (clojure.set/difference (set right) (set to-be-loaded)))])
                         right-to-left))))))))))

(river-crossing-plan)