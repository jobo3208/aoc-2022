(ns aoc-2022.day-22
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def test-input "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5")

(defn pad [s n c]
  (apply str s (apply str (repeat (- n (count s)) c))))

(defn parse-input [input]
  (let [[board _ [path]] (->> (string/split input #"\n")
                              (partition-by empty?))
        board-width (apply max (map count board))
        board (->> board
                   (map #(pad % board-width \space))
                   (mapv #(mapv {\space :empty \. :open \# :wall} %)))
        _ (def **board board)
        path (->> path
                  (partition-by (comp boolean (set "0123456789")))
                  (map (fn [cs]
                         (let [s (string/join cs)]
                           (case s
                             "L" :left
                             "R" :right
                             (Integer. s))))))]
    [board path]))

(defn transpose [m]
  (apply mapv vector m))

(defn rotate-cw [m]
  (mapv (comp vec reverse) (transpose m)))

(defn rotate-ccw [m]
  (-> m rotate-cw rotate-cw rotate-cw))

(def facing->delta {:right [0 1] :left [0 -1]
                    :down [1 0] :up [-1 0]})

(defn first-tile [row]
  (count (take-while (partial = :empty) row)))

(defn last-tile [row]
  (- (count row) (first-tile (reverse row)) 1))

(defn wrap-pos [board facing [y x]]
  (case facing
    :right [y (first-tile (nth board y))]
    :left [y (last-tile (nth board y))]
    (let [board (rotate-ccw board)
          y (- (count board) x 1)]
      (case facing
        :down [(first-tile (nth board y)) x]
        :up [(last-tile (nth board y)) x]))))

(defn move-one [board facing pos]
  (let [pos' (mapv + pos (facing->delta facing))
        tile (get-in board pos')
        [pos' tile] (if (or (nil? tile) (= :empty tile))
                      (let [pos'' (wrap-pos board facing pos)
                            tile' (get-in board pos'')]
                        [pos'' tile'])
                      [pos' tile])]
    (case tile
      :open pos'
      :wall pos)))

(defn move [board facing pos n]
  (nth (iterate (partial move-one board facing) pos) n))

(def turn
  {:left {:left :down :right :up}
   :up {:left :left :right :right}
   :right {:left :up :right :down}
   :down {:left :right :right :left}})

(defn run [input]
  (let [[board path] (parse-input input)
        _ (def *board board)
        _ (def *path path)
        start [0 (first-tile (first board))]
        state {:pos start :facing :right}
        {:keys [pos facing]} (reduce
                               (fn [{:keys [pos facing] :as state} step]
                                 (if (int? step)
                                   (assoc state :pos (move board facing pos step))
                                   (assoc state :facing (get-in turn [facing step]))))
                               state
                               path)
        facing-vals {:right 0 :down 1 :left 2 :up 3}
        [y x] pos
        password (+ (* 1000 (inc y)) (* 4 (inc x)) (facing-vals facing))]
    password))

(comment
  (run test-input)
  (run (slurp (io/resource "day-22.txt"))))

(comment
  (get-in *board [199 73])
  (count *board)
  (count (first *board)))

; came up with these descriptions of the cubes with paper and scissors

(def sample-cube
  {:face-size 4
   :face-coords [[0 2] [1 0] [1 1] [1 2] [2 2] [2 3]]
   :edges
   [[[:up 3] [:left 1] false]
    [[:up 1] [:up 2] true]
    [[:down 3] [:left 5] true]
    [[:down 2] [:down 5] true]
    [[:left 2] [:down 6] true]
    [[:right 4] [:up 6] true]
    [[:right 1] [:right 6] true]]})

(def real-cube
  {:face-size 50
   :face-coords [[0 1] [0 2] [1 1] [2 0] [2 1] [3 0]]
   :edges
   [[[:left 1] [:left 4] true]
    [[:up 1] [:left 6] false]
    [[:up 2] [:down 6] false]
    [[:right 2] [:right 5] true]
    [[:down 2] [:right 3] false]
    [[:left 3] [:up 4] false]
    [[:down 5] [:right 6] false]]})

(defn pos->face [{:keys [face-size face-coords]} [y x]]
  (let [fy (quot y face-size)
        fx (quot x face-size)
        face-map (->> face-coords
                      (map-indexed #(vector %2 (inc %1)))
                      (into {}))]
    (face-map [fy fx])))

(defn face->pos [{:keys [face-size face-coords]} face]
  (let [[fy fx] (nth face-coords (dec face))]
    [(* fy face-size) (* fx face-size)]))

(defn create-edge-map [edges]
  (->> edges
       (mapcat (fn [[e1 e2 inverse]]
                 [[e1 [e2 inverse]]
                  [e2 [e1 inverse]]]))
       (into {})))

(defn wrap-pos' [board {:keys [face-size edges] :as cube} facing [y x]]
  (let [face (pos->face cube [y x])
        dy (mod y face-size)
        dx (mod x face-size)
        edge-map (create-edge-map edges)
        [[dest-edge dest-face] inverse] (edge-map [facing face])
        coord-in-edge (case facing
                        (:down :up) dx
                        (:left :right) dy)
        coord-in-edge (if inverse
                        (- face-size coord-in-edge 1)
                        coord-in-edge)
        [dest-dy dest-dx] (case dest-edge
                            :up [0 coord-in-edge]
                            :down [(dec face-size) coord-in-edge]
                            :left [coord-in-edge 0]
                            :right [coord-in-edge (dec face-size)])
        facing' (case dest-edge
                  :up :down
                  :down :up
                  :left :right
                  :right :left)]
    [facing' (mapv + (face->pos cube dest-face) [dest-dy dest-dx])]))

(defn move-one' [board cube facing pos]
  (let [pos' (mapv + pos (facing->delta facing))
        tile (get-in board pos')
        [pos' facing' tile] (if (or (nil? tile) (= :empty tile))
                              (let [[facing' pos''] (wrap-pos' board cube facing pos)
                                    tile' (get-in board pos'')]
                                [pos'' facing' tile'])
                              [pos' facing tile])]
    (case tile
      :open [facing' pos']
      :wall [facing pos])))

(defn move' [board cube facing pos n]
  (nth (iterate (fn [[facing pos]]
                  (move-one' board cube facing pos))
                [facing pos]) n))

(comment
  (face->pos sample-cube 6)
  (wrap-pos' *board sample-cube :right [5 11])
  (wrap-pos' *board sample-cube :down [11 10]))

(defn run' [input cube]
  (let [[board path] (parse-input input)
        _ (def *board board)
        _ (def *path path)
        start [0 (first-tile (first board))]
        state {:pos start :facing :right}
        {:keys [pos facing]} (reduce
                               (fn [{:keys [pos facing] :as state} step]
                                 (if (int? step)
                                   (let [[facing' pos'] (move' board cube facing pos step)]
                                     (assoc state :pos pos' :facing facing'))
                                   (assoc state :facing (get-in turn [facing step]))))
                               state
                               path)
        facing-vals {:right 0 :down 1 :left 2 :up 3}
        [y x] pos
        password (+ (* 1000 (inc y)) (* 4 (inc x)) (facing-vals facing))]
    password))

(comment
  (run' test-input sample-cube)
  (run' (slurp (io/resource "day-22.txt")) real-cube))

; SAMPLE DATA
;
; top 3, left 1
; top 1, top 2 (inverse)
; bottom 3, left 5 (inverse)
; bottom 2, bottom 5 (inverse)
; left 2, bottom 6 (inverse)
; right 4, top 6 (inverse)
; right 1, right 6 (inverse)

; ACTUAL DATA

; left 1, left 4 (inverse)
; top 1, left 6
; top 2, bottom 6
; right 2, right 5 (inverse)
; bottom 2, right 3
; left 3, top 4
; bottom 5, right 6
