(ns aoc-2022.day-21
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def test-input "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32")

(def monkey-re #"^([a-z]+): (.+)$")

(defn parse-monkey [line]
  (let [[_ name rhs] (re-find monkey-re line)
        rhs (string/split rhs #" ")
        rhs (if (= (count rhs) 1)
              (Integer. (first rhs))
              [(first rhs) (symbol (second rhs)) (last rhs)])]
    [name rhs]))

(defn parse-input [input]
  (->> (string/split input #"\n")
       (map parse-monkey)
       (into {})))

(defn get-answer [jobs name]
  (let [ans (jobs name)]
    (if (int? ans)
      ans
      (let [[x op y] ans]
        ((resolve op) (get-answer jobs x) (get-answer jobs y))))))

(defn run [input]
  (let [jobs (parse-input input)]
    (get-answer jobs "root")))

(defn get-answer' [jobs name]
  (let [ans (jobs name)]
    (cond
      (= name "humn") "humn"
      (int? ans) ans
      :else (let [[x op y] ans]
              (list op (get-answer' jobs x) (get-answer' jobs y))))))

(defn nested-contains? [haystack needle]
  (if (= haystack needle)
    true
    (when (seq? haystack)
      (some #(nested-contains? % needle) haystack))))

(def inverse '{* /, / *, + -, - +})

(defn solve [lhs rhs]
  {:pre [(nested-contains? lhs "humn")]}
  (if (= lhs "humn")
    rhs
    (let [_ (def *lhs lhs)
          _ (def *rhs rhs)
          [op x y] lhs
          _ (def *op [op x y])
          second-arg (nested-contains? y "humn")]
      (cond
        (and (= op '-) second-arg) (solve (list '+ y rhs) x)
        (and (= op '/) second-arg) (solve (list '* y rhs) x)
        :else (let [[x y] (if second-arg [y x] [x y])]
                (solve x (list (inverse op) rhs y)))))))

; (- "humn" 10)
; (- 10 "humn")

(defn run' [input]
  (let [jobs (parse-input input)
        jobs (assoc-in jobs ["root" 1] '=)
        [_ lhs rhs] (get-answer' jobs "root")
        [lhs rhs] (if (nested-contains? lhs "humn") [lhs rhs] [rhs lhs])]
    (solve lhs rhs)))

(comment
  (run test-input)
  (run (slurp (io/resource "day-21.txt")))

  (eval (run' test-input))
  (eval (run' (slurp (io/resource "day-21.txt")))))
