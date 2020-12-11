(ns day11
  (:require
    clojure.string))

(defn parse-input [input]
  (reduce merge (map-indexed (fn [x row]
                               (into {} (map-indexed (fn [y c]
                                                       {[x y] c})
                                                     row)))
                             (clojure.string/split-lines input))))

(def dirs (for [dx [-1 0 1]
                dy [-1 0 1]
                :when (not= dx dy 0)]
            [dx dy]))

(defn occupied-adjacent-seats [seating-map [x y]]
  (->> (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) dirs)
       (map seating-map)
       (filter #{\#})
       (count)))

(defn new-state-based-on-adjacent-seats [seating-map pos c]
  (cond
    (and (= c \L) (zero? (occupied-adjacent-seats seating-map pos))) \#
    (and (= c \#) (>= (occupied-adjacent-seats seating-map pos) 4)) \L
    :else c))

(defn round [seating-map new-state-fn]
  (into {} (map (fn [[pos c]]
                  {pos (new-state-fn seating-map pos c)})
                seating-map)))

(defn find-stable [seating-map new-state-fn]
  (some
    (fn [[s1 s2]]
      (when (= s1 s2)
        s1))
    (partition 2 (iterate #(round % new-state-fn) seating-map))))

(defn count-occupied-seats [seating-map]
  (count (filter #{\#} (vals seating-map))))

(defn part1 [input]
  (count-occupied-seats (find-stable (parse-input input) new-state-based-on-adjacent-seats)))

(defn first-seat-in-dir [seating-map [x y] [dx dy]]
  (let [pos [(+ x dx) (+ y dy)]]
    (case (get seating-map pos)
      \L \L
      \# \#
      \. (recur seating-map pos [dx dy])
      nil \.)))

(defn occupied-seen-seats [seating-map pos]
  (count (filter #{\#} (map (partial first-seat-in-dir seating-map pos) dirs))))

(defn new-state-based-on-seen-seats [seating-map pos c]
  (cond
    (and (= c \L) (zero? (occupied-seen-seats seating-map pos))) \#
    (and (= c \#) (>= (occupied-seen-seats seating-map pos) 5)) \L
    :else c))

(defn part2 [input]
  (count-occupied-seats (find-stable (parse-input input) new-state-based-on-seen-seats)))
