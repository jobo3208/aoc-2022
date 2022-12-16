(ns aoc-2022.day-14
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def test-input "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(defn parse-input-line [line]
  (let [->int #(Integer. %)]
    (->> (string/split line #" -> ")
         (map #(string/split % #","))
         (map #(mapv ->int %))
         (partition 2 1)
         (map vec))))

(defn parse-input [input]
  (->> (string/split input #"\n")
       (mapcat parse-input-line)))

(defn points-on-line [line]
  {:pre [(vector? line)]}
  (let [[x1 x2] (->> line (map first) sort)
        [y1 y2] (->> line (map second) sort)]
    (for [x (range x1 (inc x2))
          y (range y1 (inc y2))]
      [x y])))

(defn down [[x y]]
  [x (inc y)])

(defn down-left [[x y]]
  [(dec x) (inc y)])

(defn down-right [[x y]]
  [(inc x) (inc y)])

(defn advance-grain [rocks sand grain]
  (let [open? #(and (not (rocks %))
                    (not (sand %)))]
    (or (first (filter open? (map #(% grain) [down down-left down-right])))
        grain)))

(defn run [input]
  (let [rock-lines (parse-input input)
        rocks (->> rock-lines
                   (mapcat points-on-line)
                   (into #{}))
        last-rock-y (apply max (map second rocks))
        free-fall? (fn [grain] (>= (second grain) last-rock-y))]
    (loop [sand #{} grain [500 0]]
      (let [grain' (advance-grain rocks sand grain)]
        (cond
          (free-fall? grain') (count sand)
          (= grain grain') (recur (conj sand grain') [500 0])
          :else (recur sand grain'))))))

(defn run-part-2 [input]
  (let [rock-lines (parse-input input)
        rocks (->> rock-lines
                   (mapcat points-on-line)
                   (into #{}))
        last-rock-y (apply max (map second rocks))
        floor-y (+ last-rock-y 2)
        on-floor? (fn [grain] (= (inc (second grain)) floor-y))]
    (loop [sand #{} grain [500 0]]
      (let [grain' (advance-grain rocks sand grain)]
        (cond
          (contains? sand [500 0]) (count sand)
          (or (on-floor? grain') (= grain grain')) (recur (conj sand grain') [500 0])
          :else (recur sand grain'))))))

(comment
  (run-part-2 test-input)
  (run-part-2 (slurp (io/resource "day-14.txt"))))
