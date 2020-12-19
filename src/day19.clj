(ns day19
  (:require
    clojure.string
    [instaparse.core :as instaparse]))

(defn parse-input [input]
  (let [[rules messages] (clojure.string/split input #"\n\n")]
    {:rules    (->> rules
                    (clojure.string/split-lines)
                    (sort))
     :messages (clojure.string/split-lines messages)}))

(defn part1 [input]
  (let [{:keys [rules messages]} (parse-input input)
        parser (instaparse/parser (clojure.string/join "\n" rules))]
    (count (remove (comp instaparse/failure? parser) messages))))

(defn part2 [input]
  (let [{:keys [rules messages]} (parse-input input)
        parser (instaparse/parser (->> rules
                                       (map (fn [line]
                                              (cond
                                                (clojure.string/starts-with? line "8:")
                                                "8: 42 | 42 8"

                                                (clojure.string/starts-with? line "11:")
                                                "11: 42 31 | 42 11 31"

                                                :else
                                                line)))
                                       (clojure.string/join "\n")))]
    (count (remove (comp instaparse/failure? parser) messages))))
