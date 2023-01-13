(ns aoc-2022.day-23
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def tiny-input ".....
..##.
..#..
.....
..##.
.....")

(def test-input "....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..")

(def real-input (slurp (io/resource "day-23.txt")))

(defn parse-input [input]
  (let [matrix (->> (string/split input #"\n")
                    (mapv vec))]
    (->> (for [y (range (count matrix))
               x (range (count (first matrix)))
               :when (= (get-in matrix [y x]) \#)]
           [y x])
         (into #{}))))

(def dir->delta
  {:n [-1 0]
   :ne [-1 1]
   :e [0 1]
   :se [1 1]
   :s [1 0]
   :sw [1 -1]
   :w [0 -1]
   :nw [-1 -1]})

(def dir->siblings
  {:n [:nw :n :ne]
   :e [:ne :e :se]
   :s [:se :s :sw]
   :w [:sw :w :nw]})

(defn propose [elves dir-order elf]
  (let [surrounding-spaces (update-vals dir->delta (partial mapv + elf))
        open? (fn [spaces]
                (not (some elves (vals spaces))))
        can-move? (fn [dir]
                    (let [sibling-spaces (select-keys surrounding-spaces (dir->siblings dir))]
                      (open? sibling-spaces)))]
    (if (open? surrounding-spaces)
      elf
      (or (->> (filter can-move? dir-order)
               first
               (get surrounding-spaces))
          elf))))

(defn get-proposals [elves dir-order]
  (map (juxt identity (partial propose elves dir-order)) elves))

(defn move [proposals]
  (let [freqs (frequencies (map second proposals))]
    (->> (map (fn [[elf proposal]]
                (if (= 1 (freqs proposal))
                  proposal
                  elf))
              proposals)
         (into #{}))))

(defn round [elves dir-order]
  (-> (get-proposals elves dir-order)
      move))

(defn get-bounding-rect [elves]
  [[(apply min (map first elves)) (apply min (map second elves))]
   [(apply max (map first elves)) (apply max (map second elves))]])

(defn count-empty-spaces-in-rect [elves]
  (let [[[y1 x1] [y2 x2]] (get-bounding-rect elves)]
    (count (for [y (range y1 (inc y2))
                 x (range x1 (inc x2))
                 :when (not (contains? elves [y x]))]
             [y x]))))

(defn run [input iterations]
  (loop [i 0
         elves (parse-input input)
         dir-order (cycle [:n :s :w :e])]
    (if (= i iterations)
      (count-empty-spaces-in-rect elves)
      (recur (inc i) (round elves (take 4 dir-order)) (drop 1 dir-order)))))

(defn run-part-2 [input]
  (loop [i 0
         elves (parse-input input)
         dir-order (cycle [:n :s :w :e])]
    (let [elves' (round elves (take 4 dir-order))]
      (if (= elves elves')
        (inc i)
        (recur (inc i) elves' (drop 1 dir-order))))))

(comment
  (parse-input tiny-input)
  (propose (parse-input tiny-input) [:n :s :w :e] [1 2])
  (round (parse-input tiny-input) [:n :s :w :e])
  (run test-input 10)
  (run real-input 10)
  (run-part-2 test-input)
  (run-part-2 real-input))
