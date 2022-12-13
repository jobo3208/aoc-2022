(ns aoc-2022.day-11
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def test-input "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(def monkey-re #"Monkey (\d+):\n  Starting items: ([0-9, ]+)\n  Operation: (.+)\n  Test: (.+)\n    If true: (.+)\n    If false: (.+)")

(defn- parse-op [op-string]
  (let [tokens (string/split op-string #" ")
        _ (assert (= (count tokens) 5))
        _ (assert (= (take 2 tokens) '("new" "=")))
        [_ _ arg1 op arg2] (map edn/read-string tokens)
        _ (assert ('#{* +} op))
        _ (assert (or (int? arg1) (= arg1 'old)))
        _ (assert (or (int? arg2) (= arg2 'old)))]
    ; this is the best way i could figure out to make arg1 and arg2 refer to
    ; the old parameter to the function below. i'm pretty sure there's gotta be
    ; a better way.
    (fn [old]
      ((resolve op) (if (= arg1 'old) old arg1)
                    (if (= arg2 'old) old arg2)))))

(defn- parse-test [test-string]
  (let [[_ n] (re-find #"^divisible by (\d+)$" test-string)
        n (Integer. n)]
    [n #(zero? (mod % n))]))

(defn- parse-throwee [throwee-string]
  (let [[_ i] (re-find #"^throw to monkey (\d+)$" throwee-string)
        i (Integer. i)]
    i))

(defn- parse-monkey-match [match]
  (let [[_ i items op test throwee-pass throwee-fail] match
        i (Integer. i)
        items (mapv #(bigint (Integer. %)) (string/split items #", "))
        op (parse-op op)
        [divisor test] (parse-test test)
        throwee-pass (parse-throwee throwee-pass)
        throwee-fail (parse-throwee throwee-fail)]
    {:i i
     :items items
     :op op
     :divisor divisor
     :test test
     :throwee-pass throwee-pass
     :throwee-fail throwee-fail
     :inspections 0}))

(defn parse-input [input]
  (mapv parse-monkey-match (re-seq monkey-re input)))

(defn run-round [monkeys]
  (loop [monkeys monkeys
         i 0]
    (if (contains? monkeys i)
      (let [monkey (monkeys i)]
        (if-let [worry (get-in monkey [:items 0])]
          (let [{:keys [op test throwee-pass throwee-fail]} monkey
                worry (op worry)
                worry (quot worry 3)
                throwee (if (test worry) throwee-pass throwee-fail)
                monkeys (-> monkeys
                            (update-in [i :inspections] inc)
                            (update-in [i :items] #(subvec % 1))
                            (update-in [throwee :items] #(conj % worry)))]
            (recur monkeys i))
          (recur monkeys (inc i))))
      monkeys)))

(defn run-round-part-2 [monkeys]
  (loop [monkeys monkeys
         i 0]
    (if (contains? monkeys i)
      (let [monkey (monkeys i)]
        (if-let [worry (get-in monkey [:items 0])]
          (let [{:keys [op divisor test throwee-pass throwee-fail]} monkey
                worry (op worry)
                worry (mod worry (reduce * (map :divisor monkeys)))
                throwee (if (test worry) throwee-pass throwee-fail)
                monkeys (-> monkeys
                            (update-in [i :inspections] inc)
                            (update-in [i :items] #(subvec % 1))
                            (update-in [throwee :items] #(conj % worry)))]
            (recur monkeys i))
          (recur monkeys (inc i))))
      monkeys)))

(defn run [input]
  (let [monkeys (parse-input input)
        monkeys (nth (iterate run-round-part-2 monkeys) 10000)
        most-active (->> monkeys
                         (sort-by :inspections)
                         (reverse)
                         (take 2))
        monkey-business (reduce * (map :inspections most-active))]
    monkey-business))

(comment
  (run (slurp (io/resource "day-11.txt"))))
