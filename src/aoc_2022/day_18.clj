(ns aoc-2022.day-18
  (:require [clojure.java.io :as io]
            [clojure.set :refer [difference]]
            [clojure.string :as string]))

(def easy-test-input "1,1,1
2,1,1")

(def test-input "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5")

(def real-input (slurp (io/resource "day-18.txt")))

(defn parse-input [input]
  (let [to-int #(Integer. %)]
    (->> (string/split input #"\n")
         (map #(string/split % #","))
         (map #(mapv to-int %)))))

(defn manhattan-distance [p q]
  (reduce + (map (comp abs -) p q)))

(defn touching? [p q]
  (= 1 (manhattan-distance p q)))

(defn run-part-1 [input]
  (let [cubes (parse-input input)
        sides (* (count cubes) 6)
        pairs (for [p cubes, q cubes] [p q])
        touching-pairs (filter (partial apply touching?) pairs)]
    (- sides (count touching-pairs))))

; an air pocket would be a point that is touching another cube on all six
; faces, but is not itself a cube in the list

(defn air-pocket? [cubes [x y z :as point]]
  {:pre [(set? cubes)]}
  (and (not (contains? cubes point))
       (contains? cubes [(dec x) y z])
       (contains? cubes [(inc x) y z])
       (contains? cubes [x (dec y) z])
       (contains? cubes [x (inc y) z])
       (contains? cubes [x y (dec z)])
       (contains? cubes [x y (inc z)])))

(defn run-part-2-attempt-1 [input]
  (let [cubes (parse-input input)
        max-x (apply max (map first cubes))
        max-y (apply max (map second cubes))
        max-z (apply max (map last cubes))
        all-points (for [x (range 1 (inc max-x))
                         y (range 1 (inc max-y))
                         z (range 1 (inc max-z))]
                     [(inc x) (inc y)  (inc z)])
        air-pockets (filter (partial air-pocket? (set cubes)) all-points)
        cubes (concat cubes air-pockets)
        sides (* (count cubes) 6)
        pairs (for [p cubes, q cubes] [p q])
        touching-pairs (filter (partial apply touching?) pairs)]
    (- sides (count touching-pairs))))

; i think the above approach only works for 1x1x1 air pockets

(defn neighbors [[x y z]]
  [[(dec x) y z]
   [(inc x) y z]
   [x (dec y) z]
   [x (inc y) z]
   [x y (dec z)]
   [x y (inc z)]])

(defn find-air-pockets [cubes l w h]
  {:pre [(set? cubes)]}
  (let [space (into #{} (for [x (range (+ l 2))
                              y (range (+ w 2))
                              z (range (+ h 2))]
                          [x y z]))]
    (loop [water #{[0 0 0]}]
      (let [all-neighbors (->> water
                               (mapcat neighbors)
                               (filter space)
                               (remove cubes)
                               (remove water))]
        (if (seq all-neighbors)
          (recur (into water all-neighbors))
          (difference space cubes water))))))

(defn run-part-2-attempt-2 [input]
  (let [cubes (parse-input input)
        l (apply max (map first cubes))
        w (apply max (map second cubes))
        h (apply max (map last cubes))
        air-pockets (find-air-pockets (set cubes) l w h)
        cubes (concat cubes air-pockets)
        sides (* (count cubes) 6)
        pairs (for [p cubes, q cubes] [p q])
        touching-pairs (filter (partial apply touching?) pairs)]
    (- sides (count touching-pairs))))

(comment
  (run-part-1 test-input)
  (run-part-1 real-input))

(comment
  (run-part-2-attempt-2 test-input)
  (run-part-2-attempt-2 real-input))
