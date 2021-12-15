(ns aoc2021.day11
  (:require [clojure.java.io :as io]
            [aoc2021.util :as u]))


(def sample
  "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(def small-sample
  "11111
19991
19191
19991
11111")


(defn neighbors [x y]
  [[(inc x) y] ;;r
   [(dec x) y] ;;l
   [x (inc y)] ;;u
   [x (dec y)] ;;d
   [(inc x) (inc y)] ;;ur
   [(dec x) (inc y)] ;;ul
   [(dec x) (dec y)] ;;dl
   [(inc x) (dec y)] ;;dr
   ])

(defn idx->xy [w h n]
  [(rem n w) (quot n h)])

(defn xy->idx [w h x y]
  (+ x (* y h)))

(defn adjacency [w h]
    (->> (for [x (range w)
               y (range h)]
           (let [nebs (->> (neighbors x y)
                           (filterv (fn [[x y]]
                                      (and (>= x 0) (< x w)
                                           (>= y 0) (< y h))))
                           (map (fn [[x y]]
                                  (xy->idx w h x y))))]
             [(xy->idx w h x y) [x y] nebs]))
         (reduce (fn [acc [n xy nebs]]
                   (assoc acc n {:coord xy :neighbors nebs})) {})))

(defn txt->state
  ([w txt]
   (let [entries (->> txt
                      (clojure.string/join " ")
                      u/read-as-vector)
         h    (long (/ (count entries) w))]
     {:entries    entries
      :adjacency (adjacency w h)
      :zeros     (->> (map-indexed vector entries)
                      (filter (fn [[idx v]]
                                (= v 0)))
                      (map first)
                      (into #{}))}))
  ([txt] (txt->state 10 txt )))

(defn naive-step [{:keys [adjacency entries] :as st}]
  (let [[zeros entries]
        (reduce-kv (fn [[zeros entries] n v]
                     (let [newv    (if (== v 9) 0 (inc v))
                           entries (assoc entries n newv)]
                       [(if (zero? newv) (conj zeros n) zeros)
                        entries])) [#{} entries] entries)]
    (loop [zeros    zeros
           pending  (seq zeros)
           entries  entries]
      (if-let [n (first pending)]
        (let [nebs        (->> n adjacency :neighbors (filter #(not (zeros %))))
              new-entries (reduce (fn [acc n]
                                    (update acc n (fn [v] (if (= v 9) 0 (inc v)))))
                                  entries nebs)
              new-zeros (filter (fn [n] (zero? (new-entries n))) nebs)]
          (recur (into zeros new-zeros)
                 (into (rest pending) new-zeros)
                 new-entries))
        (assoc st :entries entries :zeros zeros)))))

(defn show-grid [{:keys [entries]} & {:keys [n] :or {n 10}}]
  (doseq [l (partition n entries)]
    (println l)))

;;solve 10.1
(->> (io/resource "day11input.txt")
     slurp
     (txt->state 10)
     (iterate naive-step)
     (map (fn [s] (count (s :zeros #{}))))
     (take 101)
     (reduce +))

;;solve 10.2

(->> (io/resource "day11input.txt")
     slurp
     (txt->state 10)
     (iterate naive-step)
     (map-indexed  (fn [t s] [t (count (s :zeros #{}))]))
     (drop-while (fn [[t c]] (not= c 100)))
     (ffirst))
