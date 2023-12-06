(ns kyleerhabor.advent-2023.core 
  (:require [clojure.java.io :as io]))

(defn input [source]
  (slurp (io/resource source)))

(defn sum [coll]
  (reduce + 0 coll))

(defn zeroing [n]
  (or n 0))