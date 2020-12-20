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

;;;;;;;;;;;;

(defn parse-image2 [lines]
  (mapv (partial mapv identity) lines))

(defn parse-tile2 [s]
  (let [[id-line & image-lines] (clojure.string/split-lines s)]
    {(Long/parseLong (second (re-find #"(\d+)" id-line)))
     (parse-image2 image-lines)}))

(defn parse-input2 [input]
  (into {} (map parse-tile2 (clojure.string/split input #"\n\n"))))

(defn rotate [image]
  (mapv
    (fn [i]
      (vec (reverse (map (fn [row] (get row i)) image))))
    (range (count image))))

(defn rotations [image]
  (take 4 (iterate rotate image)))

(defn flip-horizontally [image]
  (vec (reverse image)))

(defn flip-vertically [image]
  (mapv
    (fn [row]
      (vec (reverse row)))
    image))

(defn flips [image]
  [image
   (flip-horizontally image)
   (flip-vertically image)
   (flip-vertically (flip-horizontally image))])

(defn rotations-and-flips [image]
  (for [rotation (rotations image)
        flip (flips rotation)]
    flip))

(defn sides2 [image]
  [(first image)
   (vec (map last image))
   (vec (reverse (last image)))
   (vec (reverse (map first image)))])

(defn find-first-corner [tiles]
  (some
    (fn [[id image]]
      (let [[top-side right-side bottom-side left-side] (sides2 image)
            other-sides (map reverse (mapcat sides2 (for [image (vals (dissoc tiles id))
                                                          image2 (rotations-and-flips image)]
                                                      image2)))]
        (when (and (not-any? #{top-side} other-sides)
                   (some #{right-side} other-sides)
                   (some #{bottom-side} other-sides)
                   (not-any? #{left-side} other-sides))
          [id image])))
    (for [[id image] tiles
          image2 (rotations image)]
      [id image2])))

(defn find-matching [tiles up-tile right-tile down-tile left-tile]
  (some
    (fn [[id image]]
      (let [[top-side right-side bottom-side left-side] (sides2 image)]
        (when (and (or (not up-tile)
                       (= top-side (reverse (nth (sides2 up-tile) 2))))
                   (or (not right-tile)
                       (= right-side (reverse (nth (sides2 right-tile) 3))))
                   (or (not down-tile)
                       (= bottom-side (reverse (nth (sides2 down-tile) 0))))
                   (or (not left-tile)
                       (= left-side (reverse (nth (sides2 left-tile) 01)))))
          [id image])))
    (for [[id image] tiles
          image2 (rotations-and-flips image)]
      [id image2])))

(defn find-image-grid [tiles]
  (:grid (reduce
           (fn [acc [x y]]
             (let [[id image] (find-matching (:tiles acc)
                                             (get-in acc [:grid [x (dec y)]])
                                             (get-in acc [:grid [(inc x) y]])
                                             (get-in acc [:grid [x (inc y)]])
                                             (get-in acc [:grid [(dec x) y]]))]
               (-> acc
                   (assoc-in [:grid [x y]] image)
                   (update :tiles dissoc id))))
           (let [first-corner (find-first-corner tiles)]
             {:grid  {[0 0] (second first-corner)}
              :tiles (dissoc tiles (first first-corner))})
           (let [grid-size (int (Math/sqrt (count tiles)))]
             (rest (for [x (range grid-size)
                         y (range grid-size)]
                     [x y]))))))

(defn remove-borders [grid]
  (zipmap (keys grid)
          (mapv (fn [image]
                  (mapv
                    (fn [row]
                      (-> row (rest) (butlast) (vec)))

                    (-> image (rest) (butlast) (vec))))
               (vals grid))))

(defn combine-tiles-horizontally [grid]
  (map (fn [y]
         (reduce
           (fn [image1 image2]
             (map
               (comp vec concat)
               image1
               image2))
           (map #(get grid [% y]) (range (inc (reduce max (map first (keys grid))))))))
       (range (inc (reduce max (map second (keys grid)))))))

(defn combine-tiles [grid]
  (vec (reduce concat (combine-tiles-horizontally grid))))

(def sea-monster "                  # \n#    ##    ##    ###\n #  #  #  #  #  #   ")

(def sea-monster-deltas
  (for [[y row] (map-indexed vector (clojure.string/split-lines sea-monster))
        [x c] (map-indexed vector row)
        :when (= \# c)]
    [x y]))

(defn sea-monster-positions [image]
  (filter
    (fn [positions]
      (every? (fn [[x y]]
                (= \# (get (get image y) x)))
              positions))
    (for [x (range (inc (count image)))
          y (range (inc (count image)))]
      (map (fn [[mx my]] [(+ mx x) (+ my y)]) sea-monster-deltas))))

(defn find-image-with-sea-monsters [image]
  (some
    (fn [image]
      (when-let [sea-monsters (seq (sea-monster-positions image))]
        {:image image
         :sea-monsters sea-monsters}))
    (rotations-and-flips image)))

(defn hash-positions [image]
  (for [x (range (inc (count image)))
        y (range (inc (count image)))
        :when (= \# (get (get image y) x))]
    [x y]))

(defn calculate-roughness [{:keys [image sea-monsters]}]
  (count (remove
           (set (reduce concat sea-monsters))
           (hash-positions image))))

(defn part2 [input]
  (-> (parse-input2 input)
      (find-image-grid)
      (remove-borders)
      (combine-tiles)
      (find-image-with-sea-monsters)
      (calculate-roughness)))
