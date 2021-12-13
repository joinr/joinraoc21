(ns aoc2021.day13
  (:require [clojure.java.io :as io]
            [aoc2021.util :as u]))

(def sample
  "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")


(defn txt->state [txt]
  (let [[pts _ folds]
           (->> txt
                clojure.string/split-lines
                (partition-by #{""}))
        points (set (map (fn [ln] (u/read-as-vector ln)) pts))
        folds  (->> folds (map (fn [ln] (clojure.string/split (re-find #"[x y]=.*" ln) #"=")))
                    (map (fn [[l r]] [l (clojure.edn/read-string r)])))]
    {:points points :folds folds}))

(defn fold-h [points yfold]
  (let [{:keys [same different]} (group-by (fn [[x y]]
                                                    (if (> y yfold )
                                                      :different
                                                      :same)) points )]
    (into (set same)
          (map (fn [[x y]]
                 (let [dy (- y yfold)
                       ynew (- yfold dy)]
                   [x ynew])) different))))

(defn fold-v [points xfold]
  (let [{:keys [same different]} (group-by (fn [[x y]]
                                             (if (> x xfold )
                                               :different
                                               :same)) points )]
    (into (set same)
          (map (fn [[x y]]
                 (let [dx (- x xfold)
                       xnew (- xfold dx)]
                   [xnew y])) different))))

(defn fold [state [direction v]]
  (case direction
    "y" (update state :points fold-h v)
    (update state :points fold-v v)))

(defn render-points [points]
  (let [[xmax ymax] (reduce (fn [[xmax ymax] [x y]]
                              [(max xmax x) (max ymax y)])
                            [Long/MIN_VALUE Long/MIN_VALUE] points)
        grid (vec (repeat (inc ymax) (vec (repeat (inc xmax) " "))))]
    (doseq [xs (reduce (fn [acc [x y]]
                         (assoc-in acc [y x] "#")) grid points)]
      (println (apply str xs)))))

;;solve 13.1
(let [{:keys [points folds] :as state}
      (->> (io/resource "day13input.txt")
           slurp
           txt->state)]
  (->> folds
       first
       (fold state)
       :points
       count))

;;solve 13.2
(let [{:keys [points folds] :as state}
      (->> (io/resource "day13input.txt")
           slurp
           txt->state)]
  (->> folds
       (reduce fold state)
       :points
       render-points))
