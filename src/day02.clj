(ns day02
  (:require
    [clojure.string]))

(defn parse-case [s]
  (let [[_ x y letter password] (re-find #"(\d+)-(\d+) ([^ ]): ([^ ]+)" s)]
    {:x        (Long/parseLong x)
     :y        (Long/parseLong y)
     :letter   (.charAt letter 0)
     :password password}))

(defn parse-input [input]
  (map parse-case (clojure.string/split-lines input)))

(defn valid1? [{:keys [x y letter password]}]
  (<= x (get (frequencies password) letter 0) y))

(defn part1 [input]
  (count (filter valid1? (parse-input input))))

(defn xor [b1 b2]
  (and (or b1 b2) (not (and b1 b2))))

(defn valid2? [{:keys [x y letter password]}]
  (xor (= (.charAt password (dec x)) letter) (= (.charAt password (dec y)) letter)))

(defn part2 [input]
  (count (filter valid2? (parse-input input))))
