(ns kyleerhabor.advent-2023.day1
  (:require
    [clojure.string :as str]
    [kyleerhabor.advent-2023.core :as core]))

(def char->int
  {\1 1
   \2 2
   \3 3
   \4 4
   \5 5
   \6 6
   \7 7
   \8 8
   \9 9})

(def word->int
  {"one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9})

(defn calibration [digits]
  (+ (* 10 (:first digits)) (:last digits)))

(defn numerics [s]
  (let [chars (->> s
                (map-indexed
                  (fn [i char]
                    {:i i
                     :value (char->int char)}))
                (filter :value))
        first (first chars)
        last (last chars)]
    {:first (:value first)
     :firsti (:i first)
     :last (:value last)
     :lasti (:i last)}))

(defn alphas [s]
  (->> word->int
    (map
      (fn [[word value]]
        {:value value
         :firsti (str/index-of s word)
         :lasti (str/last-index-of s word)}))
    ;; If first is not nil, last is not either.
    (filter :firsti)
    (reduce
      (fn [result match]
        (let [first? (< (:firsti match) (:firsti result))
              last? (> (:lasti match) (:lasti result))]
          (assoc result
            :first (if first?
                     (:value match)
                     (:first result))
            :firsti (:firsti (if first? match result))
            :last (if last?
                    (:value match)
                    (:last result))
            :lasti (:lasti (if last? match result)))))
      {:first 0
       :last 0
       :firsti Integer/MAX_VALUE
       :lasti Integer/MIN_VALUE})))

(defn part1 [puzzle]
  (->> puzzle
    str/split-lines
    (map numerics)
    (map calibration)
    core/sum))

(defn part2 [puzzle]
  (->> puzzle
    str/split-lines
    (map
      (fn [line]
        (let [numerics (numerics line)
              alphas (alphas line)]
          {:first (:first (if (< (:firsti numerics) (:firsti alphas))
                            numerics
                            alphas))
           :last (:last (if (> (:lasti numerics) (:lasti alphas))
                          numerics
                          alphas))})))
    (map calibration)
    core/sum))

(defn -main []
  (let [puzzle (core/input "puzzles/trebuchet.txt")]
    (part1 puzzle)
    (part2 puzzle)))

(comment
  (def puzzle (core/input "puzzles/trebuchet.txt"))
  
  (def line "hczsqfour3nxm5seven4"))