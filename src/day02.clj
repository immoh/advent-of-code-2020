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
