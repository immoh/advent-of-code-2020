(ns day14
  (:require
    clojure.string))

(defn parse-input [input]
  (map (fn [s]
         (cond
           (clojure.string/starts-with? s "mask")
           (let [[_ mask] (clojure.string/split s #" = ")]
             [:mask mask])

           (clojure.string/starts-with? s "mem")
           (let [[_ address value] (re-matches #"mem\[(\d+)\] = (\d+)" s)]
             [:mem (Long/parseLong address) (Long/parseLong value)])))
       (clojure.string/split-lines input)))

(defn mask-number [mask number]
  (let [number-2 (vec (reverse (Long/toString number 2)))]
    (Long/parseLong (apply str (reverse (map-indexed (fn [i m]
                                                       (if (= m \X) (get number-2 i \0) m))
                                                     (reverse mask))))
                    2)))

(defn apply-instruction [{:keys [mask] :as state} [op & args]]
  (case op
    :mask (assoc state :mask (first args))
    :mem (assoc-in state [:mem (first args)] (mask-number mask (second args)))))

(defn checksum [{:keys [mem]}]
  (reduce + (vals mem)))

(defn part1 [input]
  (checksum (reduce apply-instruction {} (parse-input input))))

(defn mask-address [mask address]
  (let [number-2 (vec (reverse (Long/toString address 2)))]
    (reverse (map-indexed (fn [i m]
                            (if (= m \0) (get number-2 i \0) m))
                          (reverse mask)))))

(defn floating-value? [value]
  (some #{\X} value))

(defn replace-first-x [value]
  (let [index (count (take-while (complement #{\X}) value))]
    [(assoc (vec value) index \0) (assoc (vec value) index \1)]))

(defn possible-values [floating-value]
  (loop [values [floating-value]]
    (if (not-any? floating-value? values)
      (map #(Long/parseLong (apply str %) 2) values)
      (recur (mapcat replace-first-x values)))))

(defn decode-writes [mask address value]
  (zipmap (possible-values (mask-address mask address)) (repeat value)))

(defn apply-instruction2 [{:keys [mask] :as state} [op & args]]
  (case op
    :mask (assoc state :mask (first args))
    :mem (update state :mem merge (apply decode-writes mask args))))

(defn part2 [input]
  (checksum (reduce apply-instruction2 {} (parse-input input))))
