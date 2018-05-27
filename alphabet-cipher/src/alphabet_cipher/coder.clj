(ns alphabet-cipher.coder)

(def alphabets
  (cycle [\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z]))

(defn char-to-num [c]
  (- (int c) (int \a)))

(defn num-to-char [n]
  (char (+ n (int \a))))

(defn encode-char [pair]
  (let [[keyword-c msg-c] pair
        index (char-to-num keyword-c)
        offset (char-to-num msg-c)]
    (first (nthnext alphabets (+ index offset)))))

(defn decode-char [pair]
  (let [[keyword-c encoded-msg-c] pair
        index (char-to-num keyword-c)]
    (num-to-char (.indexOf (take 26 (nthnext alphabets index)) encoded-msg-c))))

(defn encode [keyword message]
  (let [keyword-cycle (cycle keyword)
        char-pairs (partition 2 (interleave keyword-cycle message))]
    (apply str (map encode-char char-pairs))))

(defn decode [keyword message]
  (let [keyword-cycle (cycle keyword)
        char-pairs (partition 2 (interleave keyword-cycle message))]
    (apply str (map decode-char char-pairs))))
