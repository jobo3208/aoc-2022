(ns aoc-2022.day-20
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :as t]))

(def test-input "1
2
-3
3
-2
0
4")

(def real-input (slurp (io/resource "day-20.txt")))

(defn parse-input [input]
  (->> (string/split input #"\n")
       (map #(Integer. %))))

(defn index-of [item coll]
  (count (take-while (partial not= item) coll)))

(defn mix-num [numbers n]
  (let [l (count numbers)
        i (index-of n numbers)
        i' (cond
             (neg? (+ i n)) (dec (+ i n l))
             (>= (+ i n) l) (inc (mod (+ i n) l))
             (= (+ i n) (dec l)) 0
             (zero? (+ i n)) (dec l)
             :else (+ i n))
        removed (concat (take i numbers) (drop (inc i) numbers))
        added (concat (take i' removed) (list n) (drop i' removed))]
    added))

(defn run [input]
  (let [numbers (parse-input input)
        mixed (reduce mix-num numbers numbers)
        nth-after-zero (fn [n]
                         (->> (cycle mixed)
                              (drop-while (partial not= 0))
                              (drop n)
                              (first)))]
    (reduce + (map nth-after-zero [1000 2000 3000]))))

; whoops, turns out there are dupes in the input
; so we can't just find the index given the target number

(defn mix-num' [numbers [_ n :as id]]
  (let [l (count numbers)
        i (index-of id numbers)
        i' (cond
             (neg? (+ i n)) (dec (+ i n l))
             (>= (+ i n) l) (inc (mod (+ i n) l))
             (= (+ i n) (dec l)) 0
             (zero? (+ i n)) (dec l)
             :else (+ i n))
        removed (concat (take i numbers) (drop (inc i) numbers))
        added (concat (take i' removed) (list id) (drop i' removed))]
    added))

; this doesn't properly handle negative numbers that are bigger than the length of the list

(defn get-i' [i n l]
  (let [n (if (zero? n)
            0
            (* (mod (abs n) l) (/ n (abs n))))]
    (cond
      (neg? (+ i n)) (dec (+ i n l))
      (>= (+ i n) l) (inc (mod (+ i n) l))
      (= (+ i n) (dec l)) 0
      (zero? (+ i n)) (dec l)
      :else (+ i n))))

(defn move [numbers i n l]
  (let [i' (get-i' i n l)
        removed (concat (take i numbers) (drop (inc i) numbers))
        added (concat (take i' removed) (list n) (drop i' removed))]
    added))

(defn neighbors [numbers n]
  (let [last-num (last numbers)
        numbers (cycle numbers)
        [before after] (split-with (partial not= n) numbers)]
    [(or (last before) last-num) (second after)]))

(t/deftest neighbors-test
  (t/is (= (neighbors '(1 2 3) 1) [3 2]))
  (t/is (= (neighbors '(1 2 3) 2) [1 3]))
  (t/is (= (neighbors '(1 2 3) 3) [2 1])))

; 1 2 3 1 2 3 1 2 3

; 4 5 6 4 5 6 4 5 6 4 5 6

; 5 6 7 8 9 5 6 7 8 9 5 6 7 8 9 5 6 7 8 9
; 0 1 2 3 4 0 1 2 3 4 0 1 2 3 4 0 1 2 3 4

; 5 = 1

(defn mix-num'' [numbers [_ n :as id]]
  (let [l (count numbers)
        i (index-of id numbers)
        i' (move i n l)
        removed (concat (take i numbers) (drop (inc i) numbers))
        added (concat (take i' removed) (list id) (drop i' removed))]
    added))

(defn run' [input]
  (let [numbers (map vector (range) (parse-input input))
        mixed (reduce mix-num'' numbers numbers)
        nth-after-zero (fn [n]
                         (->> (cycle mixed)
                              (drop-while (comp (partial not= 0) second))
                              (drop n)
                              (first)
                              (second)))]
    (reduce + (map nth-after-zero [1000 2000 3000]))))

(def test-input-expected-reductions
 '((1, 2, -3, 3, -2, 0, 4)
   (2, 1, -3, 3, -2, 0, 4)
   (1, -3, 2, 3, -2, 0, 4)
   (1, 2, 3, -2, -3, 0, 4)
   (1, 2, -2, -3, 0, 3, 4)
   (1, 2, -3, 0, 3, 4, -2)
   (1, 2, -3, 0, 3, 4, -2)
   (1, 2, -3, 4, 0, 3, -2)))

(defn move''' [numbers i n l]
  (let [move* (fn [numbers i n l]
                (->> numbers
                     (remove (partial = n))
                     (cycle)
                     (drop i)
                     (drop (abs (mod n (dec l))))
                     (take (dec l))
                     (cons n)))]
    (if (neg? n)
      (reverse (move* (reverse numbers) (- l i 1) n l))
      (move* numbers i n l))))

(t/are [x y] (let [[numbers i] x
                   n (nth numbers i)
                   l (count numbers)
                   numbers' (move''' numbers i n l)]
               (= (neighbors numbers' n) y))
  ['(1 2 3) 0] [2 3]
  ['(1 2 3) 1] [1 3]
  ['(1 2 3) 2] [1 2]
  ['(4 5 6) 0] [6 5]
  ['(4 5 6) 1] [6 4]
  ['(4 5 6) 2] [5 4]
  ['(-1 -2 -3) 0] [-2 -3]
  ['(-1 -2 -3) 1] [-1 -3]
  ['(-1 -2 -3) 2] [-1 -2]
  ['(-4 -5 -6) 0] [-6 -5]
  ['(-4 -5 -6) 1] [-6 -4]
  ['(-4 -5 -6) 2] [-5 -4]
  ['(5 6 7 8 12) 4] [8 5])

(defn mix-num''' [numbers [_ n :as id]]
  (let [l (count numbers)
        i (index-of id numbers)
        move* (fn [numbers i]
                (->> numbers
                     (remove (partial = id))
                     (cycle)
                     (drop i)
                     (drop (mod (abs n) (dec l)))
                     (take (dec l))
                     (cons id)))]
    (if (neg? n)
      (reverse (move* (reverse numbers) (- l i 1)))
      (move* numbers i))))

(defn run''' [input]
  (let [numbers (map vector (range) (parse-input input))
        mixed (reduce mix-num''' numbers numbers)
        nth-after-zero (fn [n]
                         (->> (cycle mixed)
                              (drop-while (comp (partial not= 0) second))
                              (drop n)
                              (first)
                              (second)))]
    (reduce + (map nth-after-zero [1000 2000 3000]))))

(defn run'''-part-2 [input]
  (let [numbers (map (partial * 811589153) (parse-input input))
        numbers (map vector (range) numbers)
        mixed (reduce mix-num''' numbers (take (* (count numbers) 10) (cycle numbers)))
        nth-after-zero (fn [n]
                         (->> (cycle mixed)
                              (drop-while (comp (partial not= 0) second))
                              (drop n)
                              (first)
                              (second)))]
    (reduce + (map nth-after-zero [1000 2000 3000]))))

(comment
  (run''' test-input)
  (run''' (slurp (io/resource "day-20.txt")))

  (run'''-part-2 test-input)
  (run'''-part-2 (slurp (io/resource "day-20.txt")))

  (let [numbers (map vector (range) (parse-input test-input))
        mixed (->> (reductions mix-num'' numbers numbers)
                   (map #(map second %)))]
    (= mixed test-input-expected-reductions))

  (count (parse-input real-input))
  (sort (parse-input real-input))
  (count (into #{} (parse-input real-input))))
