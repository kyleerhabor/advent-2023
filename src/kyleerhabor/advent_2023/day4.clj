(ns kyleerhabor.advent-2023.day4 
  (:require [kyleerhabor.advent-2023.core :as core]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math :as math]))

(defn parse-numbers [nums]
  (map parse-long (filter (complement str/blank?) (str/split nums #" +"))))

(defn parse-puzzle [puzzle]
  (map
    (fn [line]
      (let [[card numbers] (str/split line #": ")
            [wins nums] (str/split numbers #" \| ")]
        {:card (parse-long (str/triml (subs card 5)))
         ;; "Your winnings, sir."
         :numbers (parse-numbers nums)
         :winnings (parse-numbers wins)}))
    (str/split-lines puzzle)))

(defn points [matches]
  (if (zero? matches)
    0
    (long (math/pow 2 (dec matches)))))

(defn matches [nums wins]
  (count (set/intersection nums wins)))

(defn part1 [puzzle]
  (->> (parse-puzzle puzzle)
    (map #(points (matches (set (:numbers %)) (set (:winnings %)))))
    core/sum))

(comment
  (def puzzle (core/input "puzzles/scratchcards.txt"))
  
  (def line (first (str/split-lines puzzle))))
