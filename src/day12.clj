(ns day12
  (:require
    clojure.string))

(defn parse-input [input]
  (map (fn [line]
         {:action (first line)
          :arg (Long/parseLong (subs line 1))})
       (clojure.string/split-lines input)))

(defn move-ship [{:keys [pos] :as ship} dir value]
  (assoc ship :pos (mapv + pos (case dir
                                 0  [0 (- value)]
                                 90  [value 0]
                                 180 [0 value]
                                 270 [(- value) 0]))))

(defn turn-ship [ship arg]
  (update ship :dir (fn [dir] (mod (+ dir arg) 360))))

(defn apply-instruction [{:keys [dir] :as ship} {:keys [action arg]}]
  (case action
    \N (move-ship ship 0 arg)
    \S (move-ship ship 180 arg)
    \E (move-ship ship 90 arg)
    \W (move-ship ship 270 arg)
    \L (turn-ship ship (- arg))
    \R (turn-ship ship arg)
    \F (move-ship ship dir arg)))

(defn manhattan-distance [{[x y] :pos}]
  (+ (Math/abs ^Integer x) (Math/abs ^Integer y)))

(defn part1 [input]
  (manhattan-distance (reduce apply-instruction {:pos [0 0] :dir 90} (parse-input input))))

(defn move-waypoint [{:keys [waypoint] :as ship} dir value]
  (assoc ship :waypoint (mapv + waypoint (case dir
                                           0  [0 (- value)]
                                           90  [value 0]
                                           180 [0 value]
                                           270 [(- value) 0]))))



(defn rotate-waypoint [{[x y] :waypoint :as ship} arg]
  (assoc ship :waypoint (case (mod arg 360)
                          90 [(- y) x]
                          180 [(- x) (- y)]
                          270 [y (- x)])))

(defn move-ship-to-waypoint [{:keys [pos waypoint] :as ship} arg]
  (assoc ship :pos (mapv + pos (mapv (partial * arg) waypoint))))

(defn apply-instruction2 [ship {:keys [action arg]}]
  (case action
    \N (move-waypoint ship 0 arg)
    \S (move-waypoint ship 180 arg)
    \E (move-waypoint ship 90 arg)
    \W (move-waypoint ship 270 arg)
    \L (rotate-waypoint ship (- arg))
    \R (rotate-waypoint ship arg)
    \F (move-ship-to-waypoint ship arg)))

(defn part2 [input]
  (manhattan-distance (reduce apply-instruction2 {:pos [0 0] :waypoint [10 -1]} (parse-input input))))
