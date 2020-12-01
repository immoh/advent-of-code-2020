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

(defn part2 [input]
  (let [freqs (frequencies (parse-input input))]
    (some (fn [[n1 _]]
            (some (fn [[n2 _]]
                    (when (pos? (get (update freqs n2 dec) (- 2020 n1 n2) 0))
                      (* n1 n2 (- 2020 n1 n2))))
                  (update freqs n1 dec)))
          freqs)))
