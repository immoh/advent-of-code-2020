(ns day17
  (:require
    clojure.string))

(defn parse-input [input]
  (reduce concat (map-indexed (fn [x row]
                                (keep-indexed (fn [y c]
                                                (if (= c \#)
                                                  [x y]))
                                              row))
                              (clojure.string/split-lines input))))

(defn deltas* [dimension]
  (loop [n dimension
         deltas [[]]]
    (if (pos? n)
      (recur (dec n) (for [delta deltas
                           d [-1 0 1]]
                       (conj delta d)))
      (remove (partial every? zero?) deltas))))

(def deltas (memoize deltas*))

(defn neighbors* [pos]
  (map #(mapv + pos %) (deltas (count pos))))

(def neighbors (memoize neighbors*))

(defn is-active-in-next-round? [actives pos]
  (let [active-neighbor-count (count (filter actives (neighbors pos)))]
    (or (= 3 active-neighbor-count)
        (and (actives pos) (= 2 active-neighbor-count)))))

(defn round [actives]
  (set (filter (partial is-active-in-next-round? actives)
               (concat actives (mapcat neighbors actives)))))

(defn to-dimension [n positions]
  (map (fn [pos] (into pos (repeat (- n (count pos)) 0))) positions))

(defn part1 [input]
  (nth (map count (iterate round (set (to-dimension 3 (parse-input input)))))
       6))

(defn part2 [input]
  (nth (map count (iterate round (set (to-dimension 4 (parse-input input)))))
       6))
