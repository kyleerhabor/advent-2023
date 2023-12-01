(ns kyleerhabor.advent-2023.core 
  (:require [clojure.java.io :as io]))

(defn input [source]
  (slurp (io/resource source)))