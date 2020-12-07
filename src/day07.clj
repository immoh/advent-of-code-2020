(ns day07
  (:require
    clojure.string))

(defn parse-line [line]
  (let [[_ color contents] (re-find #"(.+) bags contain (.+)" line)]
    {color (->> (clojure.string/split contents #", ")
                (map #(clojure.string/replace % "." ""))
                (map (fn [s]
                       (let [[_ bag-count color] (re-find #"(\d+) (.+) bag[s]?" s)]
                         (when color
                           {:color color
                            :count (Integer/parseInt bag-count)}))))
                (filter identity))}))

(defn parse-input [input]
  (into {} (map parse-line (clojure.string/split-lines input))))

(defn can-contain-color? [mapping color contained-color]
  (loop [colors (set (map :color (get mapping color)))]
    (when (seq colors)
      (or (colors contained-color)
          (recur (set (mapcat #(map :color (get mapping %)) colors)))))))

(defn part1 [input]
  (let [mapping (parse-input input)]
    (count (filter #(can-contain-color? mapping % "shiny gold") (keys mapping)))))

(defn part2 [input]
  (let [mapping (parse-input input)]
    (loop [total-count 0
           bags [{:color "shiny gold" :count 1}]]
      (if-let [new-bags (seq (mapcat (fn [{:keys [color count]}]
                                         (map #(update % :count * count)
                                              (get mapping color)))
                                       bags))]
        (recur (+ total-count (reduce + (map :count new-bags)))
               new-bags)
        total-count))))
