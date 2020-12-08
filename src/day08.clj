(ns day08
  (:require
    clojure.string))

(defn parse-line [line]
  (let [[op arg] (clojure.string/split line #" ")]
    [op (Integer/parseInt arg)]))

(defn parse-input [input]
  (into {} (map-indexed (fn [i line]
                          {i (parse-line line)})
                        (clojure.string/split-lines input))))

(defn execute-instruction [state [op arg]]
  (merge-with +
              state
              (case op
                "acc" {:acc arg :address 1}
                "jmp" {:address arg}
                "nop" {:address 1})))

(defn execute-program [program]
  (loop [{:keys [address acc visited] :as state} {:visited #{} :address 0 :acc 0}]
    (let [new-state (execute-instruction state (get program address))]
      (cond
        (visited (:address new-state))
        {:result :infinite-loop :acc acc}

        (= (:address new-state) (count program))
        {:result :terminated :acc (:acc new-state)}

        :else
        (recur (merge state new-state {:visited (conj visited (:address new-state))}))))))

(defn part1 [input]
  (:acc (execute-program (parse-input input))))

(defn alt-programs [program]
  (for [i (range (count program))]
    (update-in program [i 0] (fn [instruction]
                               (case instruction
                                 "jmp" "nop"
                                 "nop" "jmp"
                                 instruction)))))

(defn part2 [input]
  (let [program (parse-input input)]
    (some (fn [program]
            (let [{:keys [result acc]} (execute-program program)]
              (when (= :terminated result)
                acc)))
          (alt-programs program))))
