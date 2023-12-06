(ns kyleerhabor.advent-2023.day3 
  (:require
   [kyleerhabor.advent-2023.core :as core]
   [clojure.math :as math]
   [clojure.set :as set]
   [clojure.string :as str]))

#_{:clj-kondo/ignore [:redefined-var]}
(def empty \.)
(def gear-symbol \*)
(def numbers #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
(def normal (conj numbers empty))

(defn parse-puzzle [puzzle]
  (mapv
    (fn [line]
      (reduce
        (fn [cs chunk]
          (let [i (if-let [prior (last cs)]
                    (+ (:i prior) (count (:chunk prior)))
                    0)]
            (conj cs {:i i
                      :chunk chunk})))
        []
        (partition-by
          (fn [char]
            (cond
              (= empty char) :empty
              (numbers char) :number
              :else :symbol))
          line)))
    (str/split-lines puzzle)))

(defn numberize [n digit]
  (+ (* n 10) digit))

(defn char->int [c]
  (Character/digit c 10))

(defn chars->long [chars]
  (reduce numberize (map char->int chars)))

(defn part1 [puzzle]
  (let [parsed (parse-puzzle puzzle)
        flattened (mapv (comp vec flatten (partial map :chunk)) parsed)]
    (->> parsed
      (map-indexed
        (fn [linei line]
          (let [prior (get flattened (dec linei))
                next (get flattened (inc linei))
                length (count (or prior next))]
            (map-indexed
              (fn [chunki {:keys [i chunk]}]
                (if (numbers (first chunk))
                  (let [start (max 0 (dec i))
                        end (min length (+ i 1 (count chunk)))
                        top (some-> prior (subvec start end))
                        bottom (some-> next (subvec start end))
                        left (last (:chunk (get line (dec chunki))))
                        right (first (:chunk (get line (inc chunki))))]
                    (if (or
                          (seq (set/difference (set top) normal))
                          (seq (set/difference (set bottom) normal))
                          (and left (not (normal left)))
                          (and right (not (normal right))))
                      (chars->long chunk)))))
              line))))
      flatten
      (filter some?)
      core/sum)))

(defn adjacent? [i to size]
  (case (math/signum (- to i))
    -1.0 (>= to (dec i))
    0.0 true
    1.0 (<= to (+ i size))))

(defn adjacent-chunk? [chunk to]
  (adjacent? (:i chunk) to (count (:chunk chunk))))

(defn part2 [puzzle]
  (let [parsed (parse-puzzle puzzle)]
    (->> parsed
      (map-indexed
        (fn [linei line]
          (let [prior (get parsed (dec linei))
                next (get parsed (inc linei))]
            (map-indexed
              (fn [chunki {:keys [i chunk]}]
                (if (= gear-symbol (first chunk))
                  (let [left (get line (dec chunki))
                        right (get line (inc chunki))
                        top (filter #(adjacent-chunk? % i) prior)
                        bottom (filter #(adjacent-chunk? % i) next)
                        [a b] (->> (concat [(:chunk left)
                                            (:chunk right)]
                                     (map :chunk top) (map :chunk bottom))
                                (filter (comp numbers first)))]
                    (if (and a b)
                      (* (chars->long a) (chars->long b))))))
              line))))
      flatten
      (filter some?)
      core/sum)))

(defn -main []
  (let [puzzle (core/input "puzzles/gear-ratios.txt")]
    (part1 puzzle)))

(comment
  (def puzzle (core/input "puzzles/gear-ratios.txt"))
  
  (def line "...733.......289..262.....520..................161.462..........450.........................183............................................."))