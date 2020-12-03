(ns day03
  (:require
    [clojure.string]))

(defn parse-input [input]
  (let [lines (clojure.string/split-lines input)]
    {:trees (set (for [[y line] (map-indexed vector lines)
                       [x char] (map-indexed vector line)
                       :when (= char \#)]
                   [x y]))
     :rows (count lines)
     :columns (count (first lines))}))

(defn encountered-trees [rows columns trees slope]
  (->> (iterate #(mapv + % slope) [0 0])
       (take-while (fn [[_ y]] (< y rows)))
       (map (fn [[x y]] [(mod x columns) (mod y rows)]))
       (filter trees)
       (count)))

(defn part1 [input]
  (let [{:keys [columns rows trees]} (parse-input input)]
    (encountered-trees rows columns trees [3 1])))

(defn part2 [input]
  (let [{:keys [columns rows trees]} (parse-input input)]
    (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
         (map (partial encountered-trees rows columns trees))
         (reduce *))))
