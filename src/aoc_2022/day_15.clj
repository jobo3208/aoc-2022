(ns aoc-2022.day-15
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def test-input "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")

(def line-re #"^Sensor at x=([-0-9]+), y=([-0-9]+): closest beacon is at x=([-0-9]+), y=([-0-9]+)$")

(defn parse-line [line]
  (let [[_ & coords] (re-find line-re line)
        [sx sy bx by] (map #(Integer. %) coords)]
    [[sx sy] [bx by]]))

(defn parse-input [input]
  (->> (string/split input #"\n")
       (map parse-line)))

(defn distance [[px py] [qx qy]]
  (+ (abs (- px qx)) (abs (- py qy))))

(defn bounds [[sx sy] [bx by]]
  (let [d (distance [sx sy] [bx by])]
    [[(- sx d) (- sy d)] [(+ sx d) (+ sy d)]]))

(defn near-row? [sbm y]
  (let [min-y (-> sbm :bounds first second)
        max-y (-> sbm :bounds second second)]
    (<= min-y y max-y)))

(defn run [input y]
  (let [sbs (parse-input input)
        sb->info-map (fn [[s b]]
                       {:sensor s
                        :beacon b
                        :range (distance s b)
                        :bounds (bounds s b)})
        sbms (map sb->info-map sbs)
        near-row-sbms (filter #(near-row? % y) sbms)
        min-x (apply min (map (fn [sbm] (-> sbm :bounds first first)) near-row-sbms))
        max-x (apply max (map (fn [sbm] (-> sbm :bounds second first)) near-row-sbms))
        points-to-check (for [x (range min-x (inc max-x))] [x y])
        in-range-of? (fn [p sbm]
                       (<= (distance (:sensor sbm) p) (:range sbm)))
        beacon? (fn [p sbm] (= p (:beacon sbm)))
        cant-be-beacon? (fn [p] (some #(and (in-range-of? p %)
                                            (not (beacon? p %))) near-row-sbms))]
    (count (filter cant-be-beacon? points-to-check))))

(defn covered-segment [sbm y]
  (when (near-row? sbm y)
    (let [[sx sy] (:sensor sbm)
          dy (abs (- sy y))
          dx (- (:range sbm) dy)]
      [(- sx dx) (+ sx dx)])))

(defn contiguous? [segments]
  (loop [[[x1 x2] & ss] (sort segments)
         maximum x2]
    (if (some? x1)
      (if (<= x1 (inc maximum))
        (recur ss (max maximum x2))
        false)
      true)))

(defn contiguous-between? [segments x1 x2]
  (contiguous? (-> segments
                   (conj [(dec x1) (dec x1)])
                   (conj [(inc x2) (inc x2)]))))

(defn row-covered? [sbms y x1 x2]
  (let [object-xs (fn [obj]
                    (->> sbms
                        (map obj)
                        (filter #(= (second %) y))
                        (map first)
                        (into #{})))
        sensor-xs (object-xs :sensor)
        beacon-xs (object-xs :beacon)
        covered-segments (->> sbms
                              (map #(covered-segment % y))
                              (filter some?))
        covered-segments (concat covered-segments (map #(-> [% %]) sensor-xs))
        covered-segments (concat covered-segments (map #(-> [% %]) beacon-xs))]
    (contiguous-between? covered-segments x1 x2)))

(defn run-part-2-naive [input search-bounds]
  (let [sbs (parse-input input)
        sb->info-map (fn [[s b]]
                       {:sensor s
                        :beacon b
                        :range (distance s b)
                        :bounds (bounds s b)})
        sbms (map sb->info-map sbs)
        [[min-x min-y] [max-x max-y]] search-bounds
        points-to-check (for [x (range min-x (inc max-x)) y (range min-y (inc max-y))] [x y])
        sensor? (fn [p sbm] (= p (:sensor sbm)))
        beacon? (fn [p sbm] (= p (:beacon sbm)))
        in-range-of? (fn [p sbm]
                       (<= (distance (:sensor sbm) p) (:range sbm)))
        accounted-for? (fn [p] (some #(or (sensor? p %)
                                          (beacon? p %)
                                          (in-range-of? p %)) sbms))]
    (remove accounted-for? points-to-check)))

(defn run-part-2-attempt-2 [input search-bounds]
  (let [sbs (parse-input input)
        sb->info-map (fn [[s b]]
                       {:sensor s
                        :beacon b
                        :range (distance s b)
                        :bounds (bounds s b)})
        sbms (map sb->info-map sbs)
        [[min-x min-y] [max-x max-y]] search-bounds
        y (->> (map #(-> [% (row-covered? sbms % min-x max-x)]) (range (inc max-y)))
               (filter (comp not second))
               (ffirst))
        [x y] (first (run-part-2-naive input [[min-x y] [max-x y]]))]
    (+ (* x 4000000) y)))

(comment
  (run test-input 10)
  (run (slurp (io/resource "day-15.txt")) 2000000)
  (run-part-2-attempt-2 test-input [[0 0] [20 20]])
  (run-part-2-attempt-2 (slurp (io/resource "day-15.txt")) [[0 0] [4000000 4000000]]))
