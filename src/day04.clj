(ns day04
  (:require
    [clojure.string]))

(defn parse-passport [s]
  (into {} (map #(clojure.string/split % #":") (clojure.string/split s #" "))))

(defn parse-input [input]
  (->> (clojure.string/split-lines input)
       (partition-by clojure.string/blank?)
       (remove #(clojure.string/blank? (first %)))
       (map #(clojure.string/join " " %))
       (map parse-passport)))

(defn valid1? [passport]
  (every? (set (keys passport)) ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"] ))

(defn part1 [input]
  (count (filter valid1? (parse-input input))))

(defn valid-number-between? [s x y]
  (try
    (<= x (Integer/parseInt s) y)
    (catch NumberFormatException _)))

(defn valid-height? [s]
  (let [[_ n unit] (re-find #"(\d+)(in|cm)" s)]
    (case unit
      "cm" (valid-number-between? n 150 193)
      "in" (valid-number-between? n 59 76)
      false)))

(defn valid-hex-color? [s]
  (re-matches #"#[0-9a-f]{6}" s))

(defn valid-eye-color? [s]
  (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} s))

(defn valid-pid? [s]
  (re-matches #"\d{9}" s))

(defn valid2? [passport]
  (and (valid1? passport)
       (valid-number-between? (get passport "byr") 1920 2002)
       (valid-number-between? (get passport "iyr") 2010 2020)
       (valid-number-between? (get passport "eyr") 2020 2030)
       (valid-height? (get passport "hgt"))
       (valid-hex-color? (get passport "hcl"))
       (valid-eye-color? (get passport "ecl"))
       (valid-pid? (get passport "pid"))))

(defn part2 [input]
  (count (filter valid2? (parse-input input))))
