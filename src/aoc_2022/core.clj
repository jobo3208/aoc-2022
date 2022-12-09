(ns aoc-2022.core
  (:require [clojure.java.io :as io]
            [clojure.set :as s]
            [clojure.string :as string]))

(defn day-1 [input]
  (let [process-elf (fn [cal-strs]
                      (->> cal-strs
                           (map #(Integer. %))
                           (reduce +)))]
    (->> (string/split input #"\n")
         (partition-by (partial = ""))
         (filter #(not= "" (first %)))
         (map process-elf)
         (apply max))))

(defn day-1-part-2 [input]
  (let [process-elf (fn [cal-strs]
                      (->> cal-strs
                           (map #(Integer. %))
                           (reduce +)))]
    (->> (string/split input #"\n")
         (partition-by (partial = ""))
         (filter #(not= "" (first %)))
         (map process-elf)
         (sort)
         (reverse)
         (take 3)
         (reduce +))))

(defn day-2 [input]
  (let [char->shape {"A" :rock "B" :paper "C" :scissors
                     "X" :rock "Y" :paper "Z" :scissors}
        shape->points {:rock 1 :paper 2 :scissors 3}
        shapes->result {[:rock :rock] :draw
                        [:rock :paper] :win
                        [:rock :scissors] :loss
                        [:paper :rock] :loss
                        [:paper :paper] :draw
                        [:paper :scissors] :win
                        [:scissors :rock] :win
                        [:scissors :paper] :loss
                        [:scissors :scissors] :draw}
        result->points {:loss 0 :draw 3 :win 6}]
    (->> (string/split input #"\n")
         (map #(string/split % #" "))
         (map #(mapv char->shape %))
         (map (fn score [shapes]
                (+ (shape->points (second shapes))
                   (result->points (shapes->result shapes)))))
         (reduce +))))

(defn day-2-part-2 [input]
  (let [char->shape {"A" :rock "B" :paper "C" :scissors}
        char->result {"X" :loss "Y" :draw "Z" :win}
        shape->points {:rock 1 :paper 2 :scissors 3}
        shapes->result {[:rock :rock] :draw
                        [:rock :paper] :win
                        [:rock :scissors] :loss
                        [:paper :rock] :loss
                        [:paper :paper] :draw
                        [:paper :scissors] :win
                        [:scissors :rock] :win
                        [:scissors :paper] :loss
                        [:scissors :scissors] :draw}
        shape+result->shape (->> shapes->result
                                 (map (fn [[[shape-1 shape-2] result]]
                                        [[shape-1 result] shape-2]))
                                 (into {}))
        result->points {:loss 0 :draw 3 :win 6}]
    (->> (string/split input #"\n")
         (map #(string/split % #" "))
         (map #(-> [(char->shape (first %)) (char->result (second %))]))
         (map (fn score [[shape-1 result]]
                (let [shape-2 (shape+result->shape [shape-1 result])]
                  (+ (shape->points shape-2)
                     (result->points (shapes->result [shape-1 shape-2]))))))
         (reduce +))))

(defn day-3 [input]
  (let [get-common-item (fn [rucksack]
                          (let [[c1 c2] (split-at (/ (count rucksack) 2) rucksack)
                                items (s/intersection (set c1) (set c2))]
                            (assert (= (count items) 1))
                            (first items)))
        priority (zipmap "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" (drop 1 (range)))]
    (->> (string/split input #"\n")
         (map get-common-item)
         (map priority)
         (reduce +))))

(defn day-3-part-2 [input]
  (let [get-common-item (fn [rucksacks]
                          (let [items (apply s/intersection (map set rucksacks))]
                            (assert (= (count items) 1))
                            (first items)))
        priority (zipmap "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" (drop 1 (range)))]
    (->> (string/split input #"\n")
         (partition 3)
         (map get-common-item)
         (map priority)
         (reduce +))))

(defn day-4 [input]
  (let [parse (fn [s]
                (let [nums (string/split s #"[,-]")
                      [x1 y1 x2 y2] (map #(Integer. %) nums)]
                  [[x1 y1] [x2 y2]]))
        fully-contains? (fn [[x1 y1] [x2 y2]]
                          (and (<= x1 x2) (>= y1 y2)))]
    (->> (string/split input #"\n")
         (map parse)
         (filter (fn [[r1 r2]]
                   (or (fully-contains? r1 r2)
                       (fully-contains? r2 r1))))
         (count))))

(defn day-4-part-2 [input]
  (let [parse (fn [s]
                (let [nums (string/split s #"[,-]")
                      [x1 y1 x2 y2] (map #(Integer. %) nums)]
                  [[x1 y1] [x2 y2]]))
        overlap? (fn [[x1 y1] [x2 y2]]
                   (or (<= x1 x2 y1)
                       (<= x2 x1 y2)))]
    (->> (string/split input #"\n")
         (map parse)
         (filter (fn [[r1 r2]]
                   (overlap? r1 r2)))
         (count))))

(defn day-5 [input]
  (let [[crate-lines _ move-lines] (->> (string/split input #"\n")
                                        (partition-by (partial = "")))
        num-cols (->> (string/split (last crate-lines) #" +")
                      (filter seq)
                      (map #(Integer. %))
                      (last))
        ; 0 = 1, 1 = 5, 2 = 9, ...
        col-idx->char-idx (fn [i] (+ (* i 4) 1))
        parse-crate-line (fn [line]
                           (map #(nth line (col-idx->char-idx %)) (range num-cols)))
        transpose (fn [m] (apply mapv vector m))  ; https://stackoverflow.com/a/10347404/1395204
        crates (->> crate-lines
                    butlast
                    reverse
                    (map parse-crate-line)
                    transpose
                    (map #(take-while (partial not= \space) %))
                    (mapv vec))
        parse-move-line (fn [line]
                          (let [[q f t] (->> (re-find #"move (\d+) from (\d+) to (\d+)" line)
                                            (drop 1)
                                            (map #(Integer. %)))]
                            (repeat q [(dec f) (dec t)])))
        moves (->> move-lines
                   (mapcat parse-move-line))
        execute-move (fn [crates [f t]]
                       (let [crate (peek (nth crates f))]
                         (-> crates
                             (update f pop)
                             (update t conj crate))))
        moved-crates (reduce execute-move crates moves)]
    (string/join (map peek moved-crates))))

(defn day-5-part-2 [input]
  (let [[crate-lines _ move-lines] (->> (string/split input #"\n")
                                        (partition-by (partial = "")))
        num-cols (->> (string/split (last crate-lines) #" +")
                      (filter seq)
                      (map #(Integer. %))
                      (last))
        ; 0 = 1, 1 = 5, 2 = 9, ...
        col-idx->char-idx (fn [i] (+ (* i 4) 1))
        parse-crate-line (fn [line]
                           (map #(nth line (col-idx->char-idx %)) (range num-cols)))
        transpose (fn [m] (apply mapv vector m))  ; https://stackoverflow.com/a/10347404/1395204
        crates (->> crate-lines
                    butlast
                    reverse
                    (map parse-crate-line)
                    transpose
                    (map #(take-while (partial not= \space) %))
                    (mapv vec))
        parse-move-line (fn [line]
                          (let [[q f t] (->> (re-find #"move (\d+) from (\d+) to (\d+)" line)
                                            (drop 1)
                                            (map #(Integer. %)))]
                            [q (dec f) (dec t)]))
        moves (->> move-lines
                   (map parse-move-line))
        execute-move (fn [crates [q f t]]
                       (let [crates-to-move (take-last q (nth crates f))]
                         (-> crates
                             (assoc f (into [] (drop-last q (nth crates f))))
                             (assoc t (into (nth crates t) crates-to-move)))))
        moved-crates (reduce execute-move crates moves)]
    (string/join (map peek moved-crates))))

(defn day-6 [n input]
  (->> (map vector (partition n 1 input) (range))
       (drop-while (fn [[chs i]]
                     (not= (count (set chs)) n)))
       first
       (#(+ n (second %)))))

(defn day-7 [input]
  (let [is-command? #(string/starts-with? % "$")
        parse-command (fn [line]
                        (let [[_ command & args] (string/split line #" ")]
                          (case command
                            "cd" [:command :cd (first args)]
                            "ls" [:command :ls])))
        parse-listing (fn [line]
                        (let [xs (string/split line #" ")]
                          (case (first xs)
                            "dir" [:listing :dir (second xs)]
                            [:listing :file (second xs) (Integer. (first xs))])))
        init-state {:cwd [] :fs {}}
        process-cd (fn [state [_ _ dir]]
                     (if (= dir "..")
                       (update state :cwd pop)
                       (update state :cwd conj dir)))
        process-command (fn [state item]
                          (case (second item)
                            :cd (process-cd state item)
                            :ls state)) ; no-op
        process-listing (fn [state [_ type name size :as item]]
                          (update state :fs #(if (= type :dir)
                                               (assoc-in % (conj (:cwd state) name) {})
                                               (assoc-in % (conj (:cwd state) name) size))))
        process-item (fn [state item]
                       (case (first item)
                         :command (process-command state item)
                         :listing (process-listing state item)))
        items (->> (string/split input #"\n")
                   (map #(if (is-command? %)
                           (parse-command %)
                           (parse-listing %))))
        fs (:fs (reduce process-item init-state items))
        get-size (fn get-size [fs path]
                   (let [node (get-in fs path)]
                     (if (map? node)
                       (reduce + (map #(get-size fs (conj path %)) (keys node)))
                       node)))
        get-dir-paths (fn get-dir-paths
                        ([fs]
                         (get-dir-paths fs [] []))
                        ([m prefix result]
                         (reduce-kv
                           (fn [res k v]
                             (if (map? v)
                               (get-dir-paths v (conj prefix k) (conj res (conj prefix k)))
                               res))
                           result
                           m)))
        dir-sizes (map (partial get-size fs) (get-dir-paths fs))]
    (->> dir-sizes
         (filter #(<= % 100000))
         (reduce +))))

(defn day-7-part-2 [input]
  (let [is-command? #(string/starts-with? % "$")
        parse-command (fn [line]
                        (let [[_ command & args] (string/split line #" ")]
                          (case command
                            "cd" [:command :cd (first args)]
                            "ls" [:command :ls])))
        parse-listing (fn [line]
                        (let [xs (string/split line #" ")]
                          (case (first xs)
                            "dir" [:listing :dir (second xs)]
                            [:listing :file (second xs) (Integer. (first xs))])))
        init-state {:cwd [] :fs {}}
        process-cd (fn [state [_ _ dir]]
                     (if (= dir "..")
                       (update state :cwd pop)
                       (update state :cwd conj dir)))
        process-command (fn [state item]
                          (case (second item)
                            :cd (process-cd state item)
                            :ls state)) ; no-op
        process-listing (fn [state [_ type name size :as item]]
                          (update state :fs #(if (= type :dir)
                                               (assoc-in % (conj (:cwd state) name) {})
                                               (assoc-in % (conj (:cwd state) name) size))))
        process-item (fn [state item]
                       (case (first item)
                         :command (process-command state item)
                         :listing (process-listing state item)))
        items (->> (string/split input #"\n")
                   (map #(if (is-command? %)
                           (parse-command %)
                           (parse-listing %))))
        fs (:fs (reduce process-item init-state items))
        get-size (fn get-size [fs path]
                   (let [node (get-in fs path)]
                     (if (map? node)
                       (reduce + (map #(get-size fs (conj path %)) (keys node)))
                       node)))
        get-dir-paths (fn get-dir-paths
                        ([fs]
                         (get-dir-paths fs [] []))
                        ([m prefix result]
                         (reduce-kv
                           (fn [res k v]
                             (if (map? v)
                               (get-dir-paths v (conj prefix k) (conj res (conj prefix k)))
                               res))
                           result
                           m)))
        dir-sizes (map (partial get-size fs) (get-dir-paths fs))
        total-space 70000000
        required-unused-space 30000000
        used-space (get-size fs ["/"])
        unused-space (- total-space used-space)
        need-to-free (- required-unused-space unused-space)]
    (->> dir-sizes
         sort
         (drop-while #(< % need-to-free))
         first)))

(comment
  (day-7-part-2 (slurp (io/resource "day-7.txt"))))
