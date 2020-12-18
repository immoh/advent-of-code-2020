(ns day18
  (:require
    clojure.string))

(declare parse-expr)

(defn parse-next-group [s]
  (loop [group []
         s (rest s)
         level 1]
    (if (zero? level)
      [(parse-expr (butlast group)) s]
      (let [c (first s)]
        (recur (conj group c) (rest s) (+ level (get {\( 1 \) -1} c 0)))))))

(defn parse-next [s]
  (when-let [c (first s)]
    (case (first s)
      \space (recur (drop-while #{\space} s))
      \( (parse-next-group s)
      (\+ \*) [c (rest s)]
      (let [[n remaining] (split-with (complement #{\space}) s)]
        [(Long/parseLong (apply str n)) remaining]))))

(defn parse-expression [s]
  (->> (iterate (comp parse-next second) [nil s])
       (rest)
       (take-while identity)
       (map first)))

(defn evaluate [expr]
  (if (sequential? expr)
    (:n (reduce (fn [acc arg]
                  (if-let [op (get {\+ + \* *} arg)]
                    (assoc acc :op op)
                    (update acc :n (:op acc) (evaluate arg))))
                {:n 0 :op +}
                expr))
    expr))

(defn parse-input [input]
  (map parse-expression (clojure.string/split-lines input)))

(defn part1 [input]
  (reduce + (map evaluate (parse-input input))))

(defn change-parse-tree [expr]
  (if (sequential? expr)
    (let [[arg1 arg2] (split-with (complement #{\*}) expr)]
      (if (seq arg2)
        [(change-parse-tree arg1) \* (change-parse-tree (rest arg2))]
        (map change-parse-tree arg1)))
    expr))

(defn part2 [input]
  (reduce + (map (comp evaluate change-parse-tree) (parse-input input))))
