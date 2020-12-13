(ns day13
  (:require
    clojure.string))

(defn parse-input [input]
  (let [[earliest-time buses] (clojure.string/split-lines input)]
    {:earliest-time (Long/parseLong earliest-time)
     :buses         (keep-indexed (fn [i bus]
                                    (when-not (= bus "x")
                                      {:index i
                                       :id   (Long/parseLong bus)}))
                                  (clojure.string/split buses #","))}))


(defn wait-times [{:keys [earliest-time buses]}]
  (zipmap (map :id buses)
          (map (fn [{:keys [id]}]
                 (let [m (mod earliest-time id)]
                   (if (zero? m)
                     m
                     (- id m))))
               buses)))

(defn min-by [k xs]
  (first (sort-by k xs)))

(defn checksum [[bus wait-time]]
  (* bus wait-time))

(defn part1 [input]
  (checksum (min-by val (wait-times (parse-input input)))))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn part2 [input]
  (:remainder (let [{:keys [buses]} (parse-input input)]
               (reduce
                 (fn [{:keys [remainder modulus]} {:keys [id index]}]
                   (loop [n remainder]
                     (if (= (+ (mod n id)) (mod (- id index) id))
                       {:remainder n :modulus (lcm modulus id)}
                       (recur (+ n modulus)))))
                 (let [{:keys [index id]} (first buses)]
                   {:remainder index
                    :modulus   id})
                 (rest buses)))))
