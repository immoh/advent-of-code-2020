(ns day23
  (:require
    clojure.string))

(defn parse-input [input]
  (map #(Integer/parseInt (str %)) input))

(defn initial [cups]
  {:cup-count (count cups)
   :current   (first cups)
   :nexts     (zipmap cups (rest (cycle cups)))})

(defn mod* [n m]
  (inc (mod (dec n) m)))

(defn state [{:keys [cup-count nexts]} n]
  (get nexts n (mod* (inc n) cup-count)))

(defn next-round [{:keys [cup-count nexts current] :as acc}]
  (let [removed-cups (take 3 (rest (iterate (partial state acc) current)))
        destination-cup (first (remove (set removed-cups) (rest (iterate #(mod* (dec %) cup-count) current))))
        new-current-cup (state acc (last removed-cups))]
    (assoc acc :current new-current-cup
               :nexts (assoc nexts current new-current-cup
                                   destination-cup (first removed-cups)
                                   (last removed-cups) (state acc destination-cup)))))

(defn labels-after-1 [nexts]
  (->> (iterate nexts 1)
       (rest)
       (take-while (complement #{1}))))

(defn part1 [input]
  (->> (parse-input input)
       (initial)
       (iterate next-round)
       (drop 100)
       (first)
       (:nexts)
       (labels-after-1)
       (apply str)))

(defn initial-with-max [cups n]
  (-> (initial cups)
      (assoc-in [:nexts n] (first cups))
      (assoc-in [:nexts (last cups)] (inc (count cups)))
      (assoc :cup-count n)))

(defn part2 [input]
  (->> (initial-with-max (parse-input input) 1000000)
       (iterate next-round)
       (drop 10000000)
       (first)
       (:nexts)
       (labels-after-1)
       (take 2)
       (reduce *)))
