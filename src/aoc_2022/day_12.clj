(ns aoc-2022.day-12
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def test-input "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(defn parse-input [input]
  (let [grid-str (-> input
                     (string/replace #"S" "a")
                     (string/replace #"E" "z"))
        grid (->> (string/split grid-str #"\n")
                  (mapv (fn [line]
                          (mapv #(- (int %) 97) line))))
        height (count grid)
        width (count (first grid))
        raw-input (string/replace input #"\n" "")
        idx->point #(-> [(quot % width) (mod % width)])
        start (idx->point (string/index-of raw-input "S"))
        end (idx->point (string/index-of raw-input "E"))]
    [grid start end]))

(defn neighbors [grid position]
  (let [height (count grid)
        width (count (first grid))
        [y x] position]
    (->> [[(dec y) x]
          [y (dec x)]
          [(inc y) x]
          [y (inc x)]]
         (filter (fn [[y x]] (and (>= y 0)
                                  (>= x 0)
                                  (< y height)
                                  (< x width)))))))

(defn can-move? [grid pos dest]
  (let [pos-alt (get-in grid pos)
        dest-alt (get-in grid dest)]
    (<= dest-alt (inc pos-alt))))

(defn find-paths [grid start end]
  (loop [paths [[start]]
         paths-to-end []]
    (if (seq paths)
      (let [shortest-path-to-end (if (seq paths-to-end)
                                   (apply min (map count paths))
                                   100000)
            path (peek paths)
            paths (pop paths)
            pos (peek path)
            dests (->> (neighbors grid pos)
                       (filter (partial can-move? grid pos))
                       (filter (comp not (set path))))
            new-paths (->> dests
                           (map #(conj path %))
                           (filter #(< (count %) shortest-path-to-end)))
            reaches-end? (fn [path] (= end (peek path)))]
        (recur (into paths (remove reaches-end? new-paths))
               (into paths-to-end (filter reaches-end? new-paths))))
      paths-to-end)))

(defn find-shortest-path [grid start end]
  (loop [paths [[start]]]
    (let [[path & paths] (sort-by count paths)
          _ (prn path)
          pos (peek path)
          dests (->> (neighbors grid pos)
                     (filter (partial can-move? grid pos))
                     (filter (comp not (set path))))
          new-paths (map #(conj path %) dests)]
      (if-let [found (first (filter #(= end (peek %)) new-paths))]
        found
        (recur (into paths new-paths))))))

(defn find-shortest-path-dijkstra [grid start ends]
  ; ends is a set of valid endpoints (generalized from 1 to many for part 2)
  (let [height (count grid)
        width (count (first grid))]
    (loop [distances {start 0}
           unvisited (into #{} (for [y (range height) x (range width)] [y x]))]
      (if-let [node (->> distances
                         (filter (comp unvisited key))
                         (sort-by val)
                         (ffirst))]
        (let [distance-to-node (distances node)
              dests (->> (neighbors grid node)
                         ; swapped order for part 2
                         (filter #(can-move? grid % node))
                         (filter (comp unvisited)))
              dest-distances (into {} (map #(-> [% (inc distance-to-node)]) dests))
              distances (merge-with min distances dest-distances)
              unvisited (disj unvisited node)]
          (recur distances unvisited))
        (apply min (vals (select-keys distances ends)))))))

(defn run [input]
  (let [[grid start end] (parse-input input)
        height (count grid)
        width (count (first grid))
        ends (into #{} (for [y (range height)
                             x (range width)
                             :when (zero? (get-in grid [y x]))]
                        [y x]))
        shortest (find-shortest-path-dijkstra grid end ends)]
    shortest))

(comment
  (run (slurp (io/resource "day-12.txt"))))
