(ns day25
  (:require
    clojure.string))

(defn parse-input [input]
  (map #(Long/parseLong %) (clojure.string/split-lines input)))

(defn next-value [secret-number value]
  (rem (* value secret-number) 20201227))

(defn find-loop-size [secret-number public-key]
  (loop [value secret-number
         loop-size 1
         seen #{}]
    (when-not (seen value)
      (if (= value public-key)
        loop-size
        (recur (next-value secret-number value) (inc loop-size) (conj seen value))))))

(defn find-loop-sizes [public-key1 public-key2]
  (loop [secret-number 2]
    (let [loop-sizes (map (partial find-loop-size secret-number) [public-key1 public-key2])]
      (if (every? identity loop-sizes)
        loop-sizes
        (recur (inc secret-number))))))

(defn transform-number [loop-size secret-number]
  (first (drop loop-size (iterate (partial next-value secret-number) 1))))

(defn part1 [input]
  (let [[public-key1 public-key2] (parse-input input)]
    (transform-number
      (first (find-loop-sizes public-key1 public-key2))
      public-key2)))
