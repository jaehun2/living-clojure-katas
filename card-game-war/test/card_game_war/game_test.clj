(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))


;; fill in tests for your game
(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (is (= [:spade :ace]
           (play-round [:heart 5] [:spade :ace]))))
  (testing "queens are higher rank than jacks"
    (is (= [:club :queen]
           (play-round [:club :queen] [:club :jack]))))
  (testing "kings are higher rank than queens"
    (is (= [:club :king]
           (play-round [:club :king] [:club :queen]))))
  (testing "aces are higher rank than kings"
    (is (= [:club :ace]
           (play-round [:club :ace] [:club :king]))))
  (testing "if the ranks are equal, clubs beat spades"
    (is (= [:club 7]
           (play-round [:club 7] [:spade 7]))))
  (testing "if the ranks are equal, diamonds beat clubs"
    (is (= [:diamond 7]
           (play-round [:club 7] [:diamond 7]))))
  (testing "if the ranks are equal, hearts beat diamonds"
    (is (= [:heart 7]
           (play-round [:heart 7] [:diamond 7])))))

(deftest test-play-game
  (testing "the player loses when they run out of cards"
    (let [player1-cards [[:club 10] [:heart :king] [:heart 9] [:diamond 6] [:heart 2] [:diamond 5] [:heart 5] [:heart :jack] [:club 8] [:heart :ace] [:diamond 3] [:spade 2] [:diamond 2] [:spade :jack] [:heart 3] [:club :king] [:diamond 9] [:club :ace] [:heart 10] [:diamond :queen] [:diamond 10] [:heart 6] [:spade 6] [:diamond :jack] [:club :queen] [:club 6]]
          player2-cards [[:club 9] [:club 5] [:spade :queen] [:spade 8] [:spade 9] [:club 4] [:club 2] [:diamond 4] [:diamond :king] [:heart 8] [:spade 4] [:diamond :ace] [:spade 3] [:spade 5] [:spade :king] [:diamond 7] [:spade :ace] [:heart :queen] [:club 3] [:spade 10] [:spade 7] [:diamond 8] [:heart 4] [:club 7] [:heart 7] [:club :jack]]]
      (is (= "player1 wins."
             (play-game player1-cards player2-cards))))))

(+ 1 1)