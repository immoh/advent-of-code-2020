(ns day24
  (:require
    clojure.string))

(defn parse-next [remaining]
  (when (seq remaining)
    (let [dir (or (#{"e" "w"} (subs remaining 0 1))
                  (subs remaining 0 2))]
      [dir (subs remaining (count dir))])))

(defn parse-line [line]
  (->> (iterate (comp parse-next second) [nil line])
       (rest)
       (take-while identity)
       (map first)))

(defn parse-input [input]
  (map parse-line (clojure.string/split-lines input)))

(def dir-mapping {"e"  [0 2]
                  "se" [1 1]
                  "sw" [1 -1]
                  "w"  [0 -2]
                  "nw" [-1 -1]
                  "ne" [-1 1]})

(defn xy+ [pos1 pos2]
  (mapv + pos1 pos2))

(defn tile-pos [dirs]
  (reduce xy+ (map dir-mapping dirs)))

(defn black-tiles [tile-dirs]
  (->> tile-dirs
       (map tile-pos)
       (frequencies)
       (filter (comp odd? val))
       (keys)
       (set)))

(defn part1 [input]
  (->> (parse-input input)
       (black-tiles)
       (count)))

(defn neighbors [tile]
  (map (partial xy+ tile) (vals dir-mapping)))

(defn black-tile? [black-tiles tile]
  (let [black-neighbors (count (filter black-tiles (neighbors tile)))]
    (or (= 2 black-neighbors)
        (and (black-tiles tile) (= 1 black-neighbors)))))

(defn next-day [black-tiles]
  (->> (concat black-tiles (mapcat neighbors black-tiles))
       (filter (partial black-tile? black-tiles))
       (set)))

(defn part2 [input]
  (->> (parse-input input)
       (black-tiles)
       (iterate next-day)
       (drop 100)
       (first)
       (count)))
