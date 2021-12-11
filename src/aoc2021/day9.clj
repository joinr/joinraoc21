(ns aoc2021.day9
  (:require [clojure.java.io :as io]
            [aoc2021.util :as u]))

(def sample
  "2199943210
   3987894921
   9856789892
   8767896789
   9899965678")

;;0 always low point, 9 always high point.


;;could've done this with vectors, but assumed speed and density
;;would be useful.  meh, in retrospect it was premature optimization.
;;Probably stick with vectors next time; this is well within bounds of
;;persistent structures.  A lot of the primitive stuff is not necessary.

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn txt->grid [txt]
  (let [rows (for [ln (-> txt
                          clojure.string/split-lines)]
               (-> (clojure.string/join " " ln)
                   u/brackets
                   clojure.edn/read-string))
        w     (count (first rows))
        h     (count rows)]
    (u/new-grid w h (apply concat rows))))

(defn scan-lows [{:keys [w h] :as ^grid g}]
  (let [known  (u/new-grid w h)
        low?   (fn [^long x ^long y ^long v]
                 (or (zero?  v)
                     (and (< v 9)
                          (< v ^long (u/valid-entry g (dec x) y Long/MAX_VALUE)) ;w
                          (< v ^long (u/valid-entry g (inc x) y Long/MAX_VALUE)) ;e
                          (< v ^long (u/valid-entry g x  (inc y) Long/MAX_VALUE));s
                          (< v ^long (u/valid-entry g x (dec y) Long/MAX_VALUE)) ;n
                          )))]
    (dotimes [x w]
      (dotimes [y h]
        (let [v (get-entry g x y)]
          (when (low? x y v) (set-entry known x y (inc v))))))
    known))

;;solve 9.1
(->> (-> (io/resource "day9input.txt")
         slurp
         txt->grid
         scan-lows
         (get :entries))
     (reduce +))

;;9.2

;;looks like we are finding either strongly connected components in a graph,
;;where high points (9's) encapsulate the component.

;;Two ways to look at this:
;;find the low points, then search from there.  find descendants of
;;low points, until we reach high points.

;;The discovered set of points is the basin/component.
;;Anothe way to do it is to flood fill recursively, coloring
;;the basin.  Then traverse the graph and count each number.
;;Torn.

(defn find-basins [{:keys [w h] :as ^grid g}]
  (let [lows   (-> g scan-lows)
        points (vec (for [x (range w) y (range h)  :when (pos? (get-entry lows x y))]
                      [x y]))
        nth-grid (fn [x y]
                   (let [res (u/valid-entry g x y Long/MAX_VALUE)]
                     (when (< res Long/MAX_VALUE)
                       res)))
        neighbors (fn [x y] ;;would be faster to have a mutable focus but meh.
                    (->> [[(inc x) y]
                          [(dec x) y]
                          [x (inc y)]
                          [x (dec y)]]
                         (map (fn [[x y]]
                                (when (some-> (nth-grid x y)
                                              (< 9))
                                    [x y])))
                         (filter identity)))]
    (loop [known   {(first points) 0}
           basins  []
           roots   (rest points)
           pending (list (first points))
           idx    0]
      (if-let [next-point (first pending)]
        ;;new point to visit
        (let [xy next-point
              nebs  (filter #(not (known %)) (neighbors (xy 0) (xy 1)))]
          (recur (assoc known xy idx)
                 basins
                 roots
                 (into (rest pending) nebs)
                 idx))
        (let [new-basins (conj basins known)
              new-idx    (inc idx)]
          (if-let [root (first roots)]
            (recur {root new-idx} new-basins (rest roots) (list root) new-idx)
            new-basins))))))

;;solve 9.2
(->> (io/resource "day9input.txt")
     slurp
     txt->grid
     find-basins
     (map count)
     (sort-by -)
     (take 3)
     (reduce *))
