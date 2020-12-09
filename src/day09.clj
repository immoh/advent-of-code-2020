(ns day09
  (:require
    clojure.string))

(defn parse-input [input]
  (mapv #(Long/parseLong %) (clojure.string/split-lines input)))

(defn sum-of-two? [numbers number]
  (some #{number} (for [i (range (count numbers))
                        j (range i (count numbers))]
                    (+ (numbers i) (numbers j)))))

(defn find-invalid-number [numbers preamble-size]
  (some (fn [i]
          (when-not (sum-of-two? (mapv numbers (range (- i preamble-size) (+ i preamble-size)))
                                 (numbers i))
            (numbers i)))
        (range preamble-size (count numbers))))

(defn part1 [input]
  (find-invalid-number (parse-input input) 25))

(defn find-sequence-with-sum [numbers sum]
  (some
    #(when (= sum (reduce + %)) %)
    (for [i (range 0 (dec (count numbers)))
          j (range i (count numbers))]
      (mapv numbers (range i j)))))

(defn part2 [input sum]
  (when-let [number-sequence (find-sequence-with-sum (parse-input input) sum)]
    (+ (reduce min number-sequence) (reduce max number-sequence))))
