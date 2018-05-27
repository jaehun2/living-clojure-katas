(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn play-round [player1-card player2-card]
  (let [[suit1 rank1] player1-card
        [suit2 rank2] player2-card
        suits-order {:spade 1 :club 2 :diamond 3 :heart 4}
        rank-to-num (merge (zipmap (range 1 11) (range 1 11))
                           {:jack 11 :queen 12 :king 13 :ace 14})]
    (condp = (compare (get rank-to-num rank1) (get rank-to-num rank2))
      -1 player2-card
      0 (let [higher-suit (max (suit1 suits-order) (suit2 suits-order))]
          (condp = higher-suit
            (suit1 suits-order) player1-card
            (suit2 suits-order) player2-card))
      1 player1-card)))

(defn play-round-with-four-cards [player1-cards player2-cards]
  (let [[suit1 rank1] (last player1-cards)
        [suit2 rank2] (last player2-cards)
        suits-order {:spade 1 :club 2 :diamond 3 :heart 4}
        rank-to-num (merge (zipmap (range 1 11) (range 1 11))
                           {:jack 11 :queen 12 :king 13 :ace 14})]
    (condp = (compare (get rank-to-num rank1) (get rank-to-num rank2))
      -1 player2-cards
      0 (let [higher-suit (max (suit1 suits-order) (suit2 suits-order))]
          (condp = higher-suit
            (suit1 suits-order) player1-cards
            (suit2 suits-order) player2-cards))
      1 player1-cards)))

(let [[cards1 cards2] (partition (/ (count cards) 2) (shuffle cards))]
  (def player1-cards (vec cards1))
  (def player2-cards (vec cards2)))

(defn vrest [v]
  (vec (rest v)))

(defn cards-rest [cards drawn-cards]
  (vec (nthrest cards (count drawn-cards))))

(defn random-insert [cards card]
  (let [random-pos (rand-int (count cards))]
    (apply conj (vec (take random-pos cards)) card (vec (nthrest cards random-pos)))))

(defn play-game [player1-cards player2-cards]
  (loop [player1-cards player1-cards
         player2-cards player2-cards]
    (let [player1-card (first player1-cards)
          player2-card (first player2-cards)]
      (if (and (nil? player1-card)
               (nil? player2-card))
        "draw"
        (cond
          (nil? player1-card) "player2 wins."
          (nil? player2-card) "player1 wins."
          :else (let [winning-card (play-round player1-card player2-card)
                      next-player1-cards (vrest player1-cards)
                      next-player2-cards (vrest player2-cards)]
                  (condp = winning-card
                    player1-card (recur (conj next-player1-cards winning-card) next-player2-cards)
                    player2-card (recur next-player1-cards (conj next-player2-cards winning-card)))))))))

(defn play-game-with-four-cards [player1-cards player2-cards]
  (loop [player1-cards player1-cards
         player2-cards player2-cards]
    (let [player1-drawn-cards (vec (take 4 player1-cards))
          player2-drawn-cards (vec (take 4 player2-cards))]
      (if (and (empty? player1-drawn-cards)
               (empty? player2-drawn-cards))
        "draw"
        (cond
          (empty? player1-drawn-cards) "player2 wins."
          (empty? player2-drawn-cards) "player1 wins."
          :else (let [winning-cards (play-round-with-four-cards player1-drawn-cards player2-drawn-cards)
                      next-player1-cards (cards-rest player1-cards player1-drawn-cards)
                      next-player2-cards (cards-rest player2-cards player2-drawn-cards)]
                  (condp = winning-cards
                    player1-drawn-cards (recur (into next-player1-cards winning-cards) next-player2-cards)
                    player2-drawn-cards (recur next-player1-cards (into next-player2-cards winning-cards)))))))))

(defn play-game-random-insertion [player1-cards player2-cards]
  (loop [player1-cards player1-cards
         player2-cards player2-cards]
    (let [player1-card (first player1-cards)
          player2-card (first player2-cards)]
      (if (and (nil? player1-card)
               (nil? player2-card))
        "draw"
        (cond
          (nil? player1-card) "player2 wins."
          (nil? player2-card) "player1 wins."
          :else (let [winning-card (play-round player1-card player2-card)
                      next-player1-cards (vrest player1-cards)
                      next-player2-cards (vrest player2-cards)]
                  (condp = winning-card
                    player1-card (recur (random-insert next-player1-cards winning-card) next-player2-cards)
                    player2-card (recur next-player1-cards (random-insert next-player2-cards winning-card)))))))))

(play-game player1-cards player2-cards)
(play-game-with-four-cards player1-cards player2-cards)
(play-game-random-insertion player1-cards player2-cards)