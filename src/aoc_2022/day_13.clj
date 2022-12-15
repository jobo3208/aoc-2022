(ns aoc-2022.day-13
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def test-input "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

(defn parse-input [input]
  (->> (string/split input #"\n")
       (partition-by (partial = ""))
       (take-nth 2)
       (map #(mapv edn/read-string %))))

(defn compare-packets [p q]
  (cond
    (and (int? p) (int? q)) (compare p q)
    (and (vector? p) (vector? q)) (or (first (remove zero? (map compare-packets p q)))
                                      (compare (count p) (count q)))
    (and (vector? p) (int? q)) (compare-packets p [q])
    (and (int? p) (vector? q)) (compare-packets [p] q)))

(defn run [input]
  (let [pairs (parse-input input)
        orders (map (partial apply compare-packets) pairs)]
    (->> orders
         (map-indexed vector)
         (filter (comp neg? second))
         (map (comp inc first))
         (reduce +))))

(defn parse-input-part-2 [input]
  (->> (string/split input #"\n")
       (remove (partial = ""))
       (map edn/read-string)))

(defn run-part-2 [input]
  (let [packets (->> (parse-input-part-2 input)
                     (concat [[[2]] [[6]]])
                     (sort-by identity compare-packets))
        decoder-key (->> packets
                         (map-indexed vector)
                         (filter (comp #{[[2]] [[6]]} second))
                         (map (comp inc first))
                         (reduce *))]
    decoder-key))

(comment
  (run-part-2 (slurp (io/resource "day-13.txt"))))
