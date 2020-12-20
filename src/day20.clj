(ns day20
  (:require
    clojure.string))

(defn parse-image [lines]
  (reduce merge (map-indexed (fn [x row]
                               (into {} (map-indexed (fn [y c]
                                                       {[x y] c})
                                                     row)))
                             lines)))

(defn parse-tile [s]
  (let [[id-line & image-lines] (clojure.string/split-lines s)]
    {(Long/parseLong (second (re-find #"(\d+)" id-line)))
     {:image (parse-image image-lines)}}))

(defn parse-input [input]
  (into {} (map parse-tile (clojure.string/split input #"\n\n"))))

(defn count-unique-sides [tiles]
  (let [all-sides (mapcat :sides (vals tiles))
        side-freqs (frequencies (concat all-sides (map reverse all-sides)))]
    (into {} (map (fn [[id {:keys [sides]}]]
                    {id (count (filter #{1} (map side-freqs sides)))})
                  tiles))))

(defn sides [image]
  (let [side-length (inc (reduce max (map first (keys image))))]
    [(map #(get image [0 %]) (range side-length))
     (map #(get image [% (dec side-length)]) (range side-length))
     (map #(get image [(dec side-length) %]) (range side-length))
     (map #(get image [% 0]) (range side-length))]))

(defn add-sides [tiles]
  (into {} (map (fn [[id {:keys [image] :as tile}]]
                  {id (assoc tile :sides (sides image))})
                tiles)))

(defn part1 [input]
  (->> (parse-input input)
       (add-sides)
       (count-unique-sides)
       (filter #(= 2 (val %)))
       (keys)
       (reduce *)))


(defn part2 [input]
  (parse-input input))
