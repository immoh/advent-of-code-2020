(ns day05
  (:require
    [clojure.string]))

(defn lower-half [[x y]]
  [x (+ (/ (+ x y 1) 2) -1)])

(defn upper-half [[x y]]
  [(/ (+ x y 1) 2) y])

(defn apply-character [seat c]
  (case c
    \F (update seat :row lower-half)
    \B (update seat :row upper-half)
    \L (update seat :column lower-half)
    \R (update seat :column upper-half)))

(defn seat [boarding-pass]
  (let [seat-range (reduce apply-character {:row [0 127] :column [0 7]} boarding-pass)]
    [(first (:row seat-range)) (first (:column seat-range))]))

(defn seat-id [[row column]]
  (+ (* row 8) column))

(defn all-seat-ids [input]
  (map (comp seat-id seat) (clojure.string/split-lines input)))

(defn part1 [input]
  (reduce max (all-seat-ids input)))

(defn part2 [input]
  (let [seat-ids (set (all-seat-ids input))]
    (some
      (fn [x]
        (when (and (not (seat-ids x)) (seat-ids (dec x)) (seat-ids (inc x)))
          x))
      (range 0 (* 8 128)))))
