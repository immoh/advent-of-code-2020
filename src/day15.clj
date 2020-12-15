(ns day15
  (:require
    clojure.string))

(defn parse-input [input]
  (map #(Long/parseLong %) (clojure.string/split input #",")))

(defn play-turn [{:keys [i current last-spoken]}]
  {:i           (inc i)
   :current     (- i (last-spoken current i))
   :last-spoken (assoc last-spoken current i)})

(defn nth-spoken [numbers n]
  (->> {:i           (dec (count numbers))
        :current     (last numbers)
        :last-spoken (zipmap (butlast numbers) (range))}
       (iterate play-turn)
       (drop (- n (count numbers)))
       (first)
       (:current)))

(defn part1 [input]
  (nth-spoken (parse-input input) 2020))

(defn part2 [input]
  (nth-spoken (parse-input input) 30000000))
