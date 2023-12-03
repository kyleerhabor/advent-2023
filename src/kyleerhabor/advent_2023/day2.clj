(ns kyleerhabor.advent-2023.day2 
  (:require
   [clojure.string :as str]
   [kyleerhabor.advent-2023.core :as core]))

(defn zeroing [n]
  (or n 0))

(defn sum [coll]
  (reduce + 0 coll))

(defn product [coll]
  (reduce * coll))

(defn parse-cube [cube]
  (let [[count name] (str/split cube #" ")]
    {:name name
     :count (parse-long count)}))

(defn merge-cubes [cubes]
  (reduce
    (fn [m cube]
      (update m (:name cube) #(+ (zeroing %) (:count cube))))
    {}
    cubes))

(defn parse-game-id [label]
  (parse-long (subs label 5)))

(defn parse-game [game]
  (let [[label reveals] (str/split game #": ")]
    {:id (parse-game-id label)
     :reveals (map
                (fn [reveal]
                  (merge-cubes (map parse-cube (str/split reveal #", "))))
                (str/split reveals #"; "))}))

(defn possible? [reveals]
  (every?
    (fn [{red "red"
          green "green"
          blue "blue"}]
      (and (<= (zeroing red) 12) (<= (zeroing green) 13) (<= (zeroing blue) 14)))
    reveals))

(defn part1 [puzzle]
  (->> (str/split-lines puzzle)
    (map parse-game)
    (filter #(possible? (:reveals %)))
    (map :id)
    sum))

(defn part2 [puzzle]
  (->> (str/split-lines puzzle)
    (map parse-game)
    (map #(reduce (partial merge-with max) (:reveals %)))
    (map (comp product vals))
    sum))

(defn -main []
  (let [puzzle (core/input "puzzles/cube-conundrum.txt")]
    (part1 puzzle)
    (part2 puzzle)))

(comment
  (def puzzle (core/input "puzzles/cube-conundrum.txt"))

  (def game "Game 1: 4 green, 7 blue; 2 blue, 4 red; 5 blue, 2 green, 2 red; 1 green, 3 red, 9 blue; 3 green, 9 blue; 7 green, 2 blue, 2 red")
  
  (def reveals "4 green, 7 blue; 2 blue, 4 red; 5 blue, 2 green, 2 red; 1 green, 3 red, 9 blue; 3 green, 9 blue; 7 green, 2 blue, 2 red")
  
  (def reveal "4 green, 7 blue"))