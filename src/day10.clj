(ns day10
  (:require
    clojure.string))

(defn parse-input [input]
  (sort (map #(Long/parseLong %) (clojure.string/split-lines input))))

(defn checksum [freqs]
  (* (get freqs 1) (get freqs 3)))

(defn part1 [input]
  (let [adapters (parse-input input)]
    (->> (concat [0] adapters [(+ (last adapters) 3)])
         (partition 2 1)
         (map (fn [[x y]] (- y x)))
         (frequencies)
         (checksum))))

(declare path-count)

(defn path-count* [adapters goal value]
  (if (= value goal)
    1
    (->> [(+ value 1) (+ value 2) (+ value 3)]
         (filter adapters)
         (map (partial path-count adapters goal))
         (reduce +))))

(def path-count (memoize path-count*))

(defn part2 [input]
  (let [adapters (parse-input input)
        goal (+ (last adapters) 3)]
    (path-count (conj (set adapters) goal) goal 0)))
