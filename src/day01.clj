(ns day01
  (:require
    [clojure.string]))

(defn parse-input [input]
  (map #(Long/parseLong %) (clojure.string/split-lines input)))

(defn part1 [input]
  (let [freqs (frequencies (parse-input input))]
    (some (fn [[n _]]
            (when (pos? (get (update freqs n dec) (- 2020 n) 0))
              (* n (- 2020 n))))
          freqs)))
