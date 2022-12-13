(ns aoc-2022.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def toy-input "noop
addx 3
addx -5")

(def test-input "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

(def duration {:addx 2 :noop 1})

(defn parse-input [input]
  (->> (string/split input #"\n")
       (map (fn [line]
              (let [[op & args] (string/split line #" ")]
                (case op
                  "addx" [:addx (Integer. (first args))]
                  "noop" [:noop]))))))

(defn run-program [prog]
  (loop [[[op & args] & _ :as prog] prog
         x 1
         n 0
         op-cycles -1
         history []]
    (if (seq prog)
      (let [prog (if (= op-cycles 1) (rest prog) prog)
            history (conj history x)
            x (if (= op-cycles 1)
                (case op
                  :addx (+ x (first args))
                  x)
                x)
            n (inc n)
            op-cycles (if (> op-cycles 1) (dec op-cycles) (duration op))]
        #_ (prn prog (count prog) x n op-cycles history)
        (recur prog x n op-cycles history))
      history)))

(defn run [input]
  (let [prog (parse-input input)
        history (run-program prog)
        _ (prn history)
        indices [20 60 100 140 180 220]
        xs (map #(nth history (dec %)) indices)]
    (reduce + (map * indices xs))))

(defn run-part-2 [input]
  (let [prog (parse-input input)
        history (run-program prog)
        pixel-idxs (range (* 40 6))
        pixels (map (fn [x i] (<= (dec x) (mod i 40) (inc x))) history pixel-idxs)]
    (for [y (range 6)]
      (println (apply str (map (fn [x]
                                 (let [pixel (nth pixels (+ x (* y 40)))]
                                   (if pixel \# \.))) (range 40)))))))

(comment
  (run-part-2 (slurp (io/resource "day-10.txt"))))
