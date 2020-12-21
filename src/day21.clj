(ns day21
  (:require
    clojure.set
    clojure.string))

(defn parse-input [input]
  (map (fn [line]
         (let [[_ ingredients allergens] (re-find #"(.*) \(contains (.*)\)" line)]
           {:ingredients (set (clojure.string/split ingredients #" "))
            :allergens   (set (clojure.string/split allergens #", "))}))
       (clojure.string/split-lines input)))

(defn allergen-to-possible-ingredient [foods]
  (let [all-allergens (set (mapcat :allergens foods))]
    (zipmap all-allergens
            (map (fn [allergen]
                   (->> foods
                        (filter (fn [{:keys [allergens]}] (allergens allergen)))
                        (map :ingredients)
                        (reduce clojure.set/intersection)
                        (set)))
                 all-allergens))))

(defn combos [allergen-to-ingredient]
  (loop [allergens allergen-to-ingredient
         combos [{}]]
    (if-let [[allergen ingredients] (first allergens)]
      (recur (rest allergens)
             (for [combo combos
                   ingredient ingredients
                   :when (not-any? #{ingredient} (vals combo))]
               (assoc combo allergen ingredient)))
      combos)))

(defn part1 [input]
  (let [foods (parse-input input)
        used-ingredients (->> foods
                              (allergen-to-possible-ingredient)
                              (combos)
                              (first)
                              (vals)
                              (set))]
    (count (remove used-ingredients (mapcat :ingredients foods)))))

(defn part2 [input]
  (let [foods (parse-input input)]
    (->> foods
         (allergen-to-possible-ingredient)
         (combos)
         (first)
         (sort)
         (vals)
         (clojure.string/join ","))))
