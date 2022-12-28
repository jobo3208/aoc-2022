(ns aoc-2022.day-17
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

(tufte/add-basic-println-handler! {})

(def test-input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(def shapes
  {:dash [[0 0] [1 0] [2 0] [3 0]]
   :plus [[1 0] [0 1] [1 1]  [2 1] [1 2]]
   :angle [[0 0] [1 0] [2 0] [2 1] [2 2]]
   :pipe [[0 0] [0 1] [0 2] [0 3]]
   :block [[0 0] [1 0] [0 1] [1 1]]})

(defn move-left [shape]
  (mapv (fn [[x y]] [(dec x) y]) shape))

(defn move-right [shape]
  (mapv (fn [[x y]] [(inc x) y]) shape))

(defn move-down [shape]
  (mapv (fn [[x y]] [x (dec y)]) shape))

(defn move-to-start [peak shape]
  (mapv (fn [[x y]] [(+ x 2) (+ y peak 4)]) shape))

(defn blocked? [fallen shape]
  (p ::blocked? (some (fn [[x y]]
                        (or (neg? x) (neg? y) (> x 6) (fallen [x y]))) shape)))

(defn draw-fallen [fallen]
  (->> (for [y (range (inc (apply max (map second fallen))))]
        (apply str (for [x (range 7)]
                    (if (fallen [x y]) "#" "."))))
       reverse
       (interpose "\n")
       (apply str)))

(defn run [input iterations]
  (let [input (string/trim input)
        jet-seq (cycle input)
        rock-seq (cycle [:dash :plus :angle :pipe :block])
        jet->fn {\< move-left \> move-right}]
    (loop [i 0
           k 0
           [j & js] jet-seq
           [r & rs :as rrs] rock-seq
           fallen #{}
           peak -1
           shape nil
           history #{}]
      (let [history (if (nil? shape)
                      ; TODO: maybe try including in here a "snapshot" of the
                      ; last n rows of the structure.. if we are cycling, those
                      ; snapshots should match
                      (let [snapshot (->> fallen
                                          (map (fn [[x y]] [x (- y (- peak 10))]))
                                          (filter (fn [[x y]] (pos? y))))
                            record [(mod k (count input)) r snapshot]]
                        (when (contains? history record)
                          (prn record))
                        (conj history record))
                      history)
            shape (or shape (move-to-start peak (shapes r)))]
        #_ (prn i j r fallen shape)
        #_ (when (seq fallen) (println (draw-fallen fallen)))
        #_ (read-line)
        (if (= i iterations)
          (inc (apply max (map second fallen)))
          (let [move-horizontal (jet->fn j)
                shape' (move-horizontal shape)
                shape' (if (blocked? fallen shape') shape shape')
                shape'' (move-down shape')]
            (if (blocked? fallen shape'')
              (let [fallen (p ::add-to-fallen (into fallen shape'))
                    peak (apply max peak (map second shape'))
                    ; trim fallen every so often to prevent OOM
                    fallen (if (zero? (mod i 100000))
                             (into #{} (filter (fn [[x y]] (> y (- peak 1000))) fallen))
                             fallen)]
                ; trying to see if a straightforward multiplication was used to
                ; reach this answer.. it would appear not
                #_ (when (zero? (p ::mod (mod 1514285714288 (inc peak))))
                     (prn (inc i) j r (inc peak)))
                (recur (inc i) (inc k) js rs fallen peak nil history))
              (recur i (inc k) js rrs fallen peak shape'' history))))))))

; the key to part 2 has to be identifying the point when the process starts to cycle, and extrapolating from there

(defn run-basic [input num-rocks]
  (let [input (string/trim input)
        jet-seq (cycle input)
        rock-seq (cycle [:dash :plus :angle :pipe :block])
        jet->fn {\< move-left \> move-right}]
    (loop [i 0
           [j & js] jet-seq
           [r & rs :as rrs] rock-seq
           fallen #{}
           peak -1
           shape nil]
      (let [shape (or shape (move-to-start peak (shapes r)))]
        (if (= i num-rocks)
          fallen
          (let [move-horizontal (jet->fn j)
                shape' (move-horizontal shape)
                shape' (if (blocked? fallen shape') shape shape')
                shape'' (move-down shape')]
            (if (blocked? fallen shape'')
              (let [fallen (into fallen shape')
                    peak' (apply max peak (map second shape'))]
                (do
                  (println (- peak' peak))
                  (recur (inc i) js rs fallen peak' nil)))
              (recur i js rrs fallen peak shape''))))))))

(defn lazy-peak-deltas [input]
  (let [input (string/trim input)
        jet-seq (cycle input)
        rock-seq (cycle [:dash :plus :angle :pipe :block])
        jet->fn {\< move-left \> move-right}]
    ((fn step [i [j & js] [r & rs :as rrs] fallen peak shape]
       (lazy-seq
         (let [shape (or shape (move-to-start peak (shapes r)))
               move-horizontal (jet->fn j)
               shape' (move-horizontal shape)
               shape' (if (blocked? fallen shape') shape shape')
               shape'' (move-down shape')]
           (if (blocked? fallen shape'')
             (let [fallen (into fallen shape')
                   peak' (apply max peak (map second shape'))]
               (cons (- peak' peak) (step (inc i) js rs fallen peak' nil)))
             (step i js rrs fallen peak shape'')))))
     0 jet-seq rock-seq #{} -1 nil)))

(defn find-cycle [xs]
  (let [total-length (count xs)]
    (loop [part-length (quot total-length 2)
           start 0]
      (when (> part-length 1)
        (let [xs (drop start xs)
              sublists (partition part-length xs)]
          (if (apply = sublists)
            [start part-length]
            (if (< start (- total-length (* 2 part-length)))
              (recur part-length (inc start))
              (recur (dec part-length) 0))))))))

(defn peak-at [peak-deltas-seq start period n]
  (let [dividend (- n start)
        initial-peak (reduce + (take start peak-deltas-seq))
        pattern (->> peak-deltas-seq (drop start) (take period))
        pattern-peak (reduce + pattern)
        offset-in-pattern (mod dividend period)
        bulk (+ initial-peak (* pattern-peak (quot dividend period)))
        extra (reduce + (take offset-in-pattern pattern))]
    (+ bulk extra)))


(comment
  ;;;; final answer to part 2 was found this way:

  (->> (lazy-peak-deltas (slurp (io/resource "day-17.txt")))
       (take 10000)
       (find-cycle))
  ; took forever, but this eventually produced:
  ; [72 3490]

  ; extract the repeating cycle
  (def real-input-pattern (->> (lazy-peak-deltas (slurp (io/resource "day-17.txt")))
                               (drop 72)
                               (take 3490)))

  ; verify that it appears to repeat indefinitely
  (= real-input-pattern (->> (lazy-peak-deltas (slurp (io/resource "day-17.txt")))
                             (drop 72)
                             (drop 3490)
                             (drop 3490)
                             (drop 3490)
                             (drop 3490)
                             (drop 3490)
                             (drop 3490)
                             (drop 3490)
                             (drop 3490)
                             (drop 3490)
                             (take 3490)))

  ; here's the final answer
  (peak-at (lazy-peak-deltas (slurp (io/resource "day-17.txt"))) 72 3490 1000000000000))

(comment
  ;;;; scratch

  (->> (lazy-peak-deltas test-input)
       (take 300)
       (partition 5)
       (map (partial reduce +)))

  ; after 30 have fallen, the peak is 51
  ; every 35 beyond the initial 30, the peak grows by 53

  (def test-input-pattern (->> (lazy-peak-deltas test-input)
                               (drop 30)
                               (take 35)
                               (vec)))

  (->> (lazy-peak-deltas test-input)
       (take 30)
       (partition 5)
       (map (partial reduce +)))

  (peak-at 1000000000000)

  ; ok, that works for the test data. let's try the real data

  (doseq [period (reverse (range 3 50))
          start (range 0 1000)]
    (->> (lazy-peak-deltas (slurp (io/resource "day-17.txt")))
         (drop start)
         (take 10000)
         (partition period)
         (map (partial reduce +))
         (partition 4)
         (frequencies)
         (filter (fn [[k v]] (> v 2)))
         (#(when (seq %)
             (println start period))))

    (->> (lazy-peak-deltas (slurp (io/resource "day-17.txt")))
         (take 10000)
         (map-indexed #(str (inc %1) "," %2))
         (interpose "\n")
         (apply str)
         (spit "/tmp/blah.csv"))


    (->> (lazy-peak-deltas (slurp (io/resource "day-17.txt")))
        (drop 97680)
        (take 97855)
        (partition 140)
        (frequencies)
        (filter (fn [[k v]] (> v 2))))

   (->> (lazy-peak-deltas (slurp (io/resource "day-17.txt")))
        (drop 97860)
        (take 1000000)
        (partition (- 97855 140))
        (map (partial reduce +))))

  (- 98000 140)

  ; i'm pretty sure the period is 140

  (run test-input 100)

  (run (slurp (io/resource "day-17.txt")) 2022)

  (map (partial run test-input) (range 1 20))

  (def *first-ht (into [] (take 100000 (lazy-peak-deltas (slurp (io/resource "day-17.txt"))))))

  ; attempting to work backwards to find a possible cycle point
  ; this would be much more efficient if we could implement as a lazy-seq
  ; possibly helpful: <https://stackoverflow.com/questions/21712145/lazy-sequence-using-loop-recur>
  (->> (range 1 10000)
       (map (partial run test-input))
       (filter #(zero? (mod 1514285714288 %)))
       (take 10)))
