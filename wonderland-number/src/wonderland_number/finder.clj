(ns wonderland-number.finder)

(defn sameDigits? [n1 n2]
  (let [s1 (set (str n1))
        s2 (set (str n2))]
    (= s1 s2)))

(defn wonderland-number []
  (let [numbers (range 100000 (int (/ 1000000 6.0)))
        pred (fn [x]
               (and (sameDigits? x (* 2 x))
                    (sameDigits? x (* 3 x))
                    (sameDigits? x (* 4 x))
                    (sameDigits? x (* 5 x))
                    (sameDigits? x (* 6 x))))]
    (some #(when (pred %) %) numbers)))

(defn sumOfTheCubesOfDigit [n]
  (reduce (fn [sum ch]
            (let [digit (- (int ch) (int \0))]
              (+ sum (* digit digit digit))))
          0
          (str n)))

(defn cube-number []
  (let [numbers (range 1 1000)
        pred #(= % (sumOfTheCubesOfDigit %))]
    (filter pred numbers)))