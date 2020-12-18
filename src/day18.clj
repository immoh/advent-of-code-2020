(ns day18
  (:require
    clojure.string))

(defn parse-expression [s]
  (loop [s s
         expr []
         res []
         state :whitespace
         level 0]
    (if-let [c (first s)]
      (case state
        :whitespace  (case c
                       \( (recur (rest s) expr res :parenthesis (inc level))
                       \space (recur (rest s) expr res state level)
                       (\+ \*) (recur (rest s) expr (conj res c) state level)
                       (recur (rest s) (conj expr c) res :number level))
        :parenthesis (case c
                       \( (recur (rest s) (conj expr c) res state (inc level))
                       \) (if (= level 1)
                            (recur (rest s) [] (conj res (parse-expression (apply str expr))) :whitespace 0)
                            (recur (rest s) (conj expr c) res state (dec level)))
                       (recur (rest s) (conj expr c) res state level))
        (if (= c \space)
          (recur (rest s) [] (conj res (Long/parseLong (apply str expr))) :whitespace level)
          (recur (rest s) (conj expr c) res :number level)))
      (if (seq expr)
        (conj res (Long/parseLong (apply str expr)))
        res))))

(defn evaluate [expr]
  (loop [expr expr
         acc 0
         op +]
    (let [arg (first expr)]
      (if arg
        (cond
          (sequential? arg)
          (recur (rest expr) (op acc (evaluate arg)) op)

          (#{\+ \*} arg)
          (recur (rest expr) acc (get {\+ + \* *} arg))

          :else
          (recur (rest expr) (op acc arg) op))
        acc))))

(defn parse-input [input]
  (map parse-expression (clojure.string/split-lines input)))

(defn part1 [input]
  (reduce + (map evaluate (parse-input input))))

(defn evaluate2 [expr]
  (cond
    (some sequential? expr)
    (evaluate2 (map #(if (sequential? %)
                       (evaluate2 %)
                       %)
                    expr))

    (some #{\*} expr)
    (let [[arg1 arg2] (split-with (complement #{\*}) expr)]
      (* (evaluate2 arg1) (evaluate2 (rest arg2))))
    :else
    (evaluate expr)))

(defn part2 [input]
  (reduce + (map evaluate2 (parse-input input))))
