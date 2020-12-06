(ns day06
  (:require
    [clojure.set]
    [clojure.string]))

(defn parse-input [input]
  (->> (clojure.string/split input #"\n\n")
       (map clojure.string/split-lines)
       (map #(map set %))))

(defn part1 [input]
  (->> (parse-input input)
       (map #(reduce clojure.set/union %))
       (map count)
       (reduce +)))

(defn part2[input]
  (->> (parse-input input)
       (map #(reduce clojure.set/intersection %))
       (map count)
       (reduce +)))
