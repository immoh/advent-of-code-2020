(ns day16
  (:require
    clojure.string
    clojure.set))

(defn parse-range [s]
  (map #(Long/parseLong %) (clojure.string/split s #"-")))

(defn parse-field [s]
  (let [[name ranges] (clojure.string/split s #": ")]
    {name (map parse-range (clojure.string/split ranges #" or "))}))

(defn parse-ticket [s]
  (zipmap (range) (map #(Long/parseLong %) (clojure.string/split s #","))))

(defn parse-input [input]
  (let [[fields your-ticket nearby-tickets] (clojure.string/split input #"\n\n")]
    {:fields         (into {} (map parse-field (clojure.string/split-lines fields)))
     :your-ticket    (parse-ticket (second (clojure.string/split-lines your-ticket)))
     :nearby-tickets (map parse-ticket (rest (clojure.string/split-lines nearby-tickets)))}))

(defn value-in-range? [value [x y]]
  (<= x value y))

(defn valid-field? [value [_ ranges]]
  (some (partial value-in-range? value) ranges))

(defn valid-value? [fields value]
  (some (partial valid-field? value) fields))

(defn invalid-values [fields ticket]
  (remove (partial valid-value? fields) (vals ticket)))

(defn part1 [input]
  (let [{:keys [fields nearby-tickets]} (parse-input input)]
    (reduce + (mapcat (partial invalid-values fields) nearby-tickets))))

(defn possible-fields [fields value]
  (set (map key (filter (partial valid-field? value) fields))))

(defn narrow-down-fields [possible-fields]
  (if (every? #{1} (map count possible-fields))
    (map first possible-fields)
    (let [known-fields (set (map first (filter #(= 1 (count %)) possible-fields)))]
      (recur (map (fn [fields]
                    (if (= 1 (count fields))
                      fields
                      (clojure.set/difference fields known-fields)))
                  possible-fields)))))

(defn determine-field-mapping [fields valid-tickets]
  (let [initial-field-mapping (into {}
                                    (map
                                      (fn [i]
                                        {i (->> (map #(get % i) valid-tickets)
                                                (map (partial possible-fields fields))
                                                (reduce clojure.set/intersection))})
                                      (range (count (first valid-tickets)))))]
    (zipmap (keys initial-field-mapping) (narrow-down-fields (vals initial-field-mapping)))))

(defn valid-ticket? [fields ticket]
  (not (seq (invalid-values fields ticket))))

(defn part2 [input]
  (let [{:keys [fields your-ticket nearby-tickets]} (parse-input input)
        valid-tickets (filter  (partial valid-ticket? fields) nearby-tickets)
        field-mapping (determine-field-mapping fields valid-tickets)]
    (reduce * (map val (filter #(clojure.string/starts-with? (key %) "departure")
                               (clojure.set/rename-keys your-ticket field-mapping))))))
