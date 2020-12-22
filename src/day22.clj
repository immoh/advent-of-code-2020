(ns day22)

(defn parse-input [input]
  (map (fn [player]
         (vec (map #(Long/parseLong %) (rest (clojure.string/split-lines player)))))
       (clojure.string/split input #"\n\n")))

(defn round [[[card1 & rest1] [card2 & rest2]]]
  (when (and card1 card2)
    (if (< card1 card2)
      [rest1 (into (vec rest2) [card2 card1])]
      [(into (vec rest1) [card1 card2]) rest2])))

(defn score [deck]
  (reduce + (map * (reverse deck) (rest (range)))))

(defn game-continues? [decks]
  (every? seq decks))

(defn part1 [input]
  (->> (parse-input input)
       (iterate round)
       (drop-while game-continues?)
       (first)
       (some seq)
       (score)))

(declare game2)

(defn round2 [{seen :seen [[card1 & rest1 :as deck1] [card2 & rest2 :as deck2] :as decks] :decks}]
  (when (and (seq decks) (every? seq decks))
    {:seen  (conj seen decks)
     :decks (if (seen decks)
              [deck1 nil]
              (let [winner (if (and (< card1 (count deck1))
                                    (< card2 (count deck2)))
                             (let [[res1] (game2 [(vec (take card1 rest1)) (vec (take card2 rest2))])]
                               (if (seq res1) 1 2))
                             (if (< card1 card2) 2 1))]
                (if (= 1 winner)
                  [(into (vec rest1) [card1 card2]) (vec rest2)]
                  [(vec rest1) (into (vec rest2) [card2 card1])])))}))

(defn game2 [decks]
  (->> {:seen #{} :decks decks}
       (iterate round2)
       (drop-while (comp game-continues? :decks))
       (first)
       (:decks)))

(defn part2 [input]
  (score (some seq (game2 (parse-input input)))))
