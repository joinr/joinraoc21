(ns aoc2021.day15
  (:require [clojure.java.io :as io]
            [aoc2021.util :as u]))

(def sample
  "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")

(defn min-pq [& xs]
  (into (sorted-set-by (fn [l r]
                         (let [res (compare (l 0) (r 0))]
                           (if-not (zero? res) res
                                   (compare (l 1) (r 1)))))) xs))

(defn peek-min [pq]
  (when-let [v (first pq)]
    (v 1)))

(defn pop-min [pq]
  (if-let [k (first pq)]
    (disj pq k)
    pq))

(defn multipath [x y]
  (if (set? x)
    (conj x y)
    (conj #{x} y)))

(defn relax [{:keys [spt dist fringe] :as state} source sink w]
  (let [dfrom (or (dist source) (throw (ex-info "unknown weight!" {:in dist :source source})))
        wnew  (+ dfrom w)]
    (if-let [wold (dist sink)] ;;known path.
      (cond (< wnew wold) ;;shorter path
            {:spt    (assoc spt sink source)
             :dist   (assoc dist sink wnew)
             :fringe (conj fringe [wnew sink])}
            (= wnew wold) ;;equal path
            {:spt  (update spt sink multipath source)
             :dist dist
             :fringe fringe}
            :else ;;no change
            state)
      ;;new path
      {:spt    (assoc spt  sink source)
       :dist   (assoc dist sink wnew)
       :fringe (conj fringe [wnew sink])})))

(defn best-first [gr from to]
  (let [sinks (gr :sinks)
        nodes (gr :nodes)
        weightf (fn [nd]
                  ((nodes nd) :risk))
        init {:spt  {from from}
              :dist {from (weightf from)}
              :fringe (min-pq [0 from])}]
    (loop [{:keys [spt dist fringe] :as state} init]
      (if-let [source (peek-min fringe)]
        (if (= source to)
          (assoc state :found-path [from to])
          (let [neighbors  (sinks source)
                next-state (reduce-kv (fn [acc sink w]
                                        (relax acc source sink w))
                                      (update state :fringe pop-min)
                                      (sinks source))]
            (recur next-state)))
        state))))

;;reuse day 11 graph helpers, now in util.  could make the graph implicit, but meh.
(defn entries->graph [entries w h & {:keys [neighbors-fn] :or {neighbors-fn u/neighbors4}}]
  (let [adj (u/adjacency w h :neighbors-fn neighbors-fn)]
    (reduce-kv (fn [acc nd {:keys [coord neighbors]}]
                 (-> acc
                     (assoc-in [:nodes nd] {:risk  (entries nd)
                                            :coord coord})
                     (assoc-in [:sinks nd]
                               (reduce (fn [acc k]
                                         (assoc acc k (entries k))) {} neighbors))))
               {:width w :height h} adj)))

(defn txt->graph [txt & {:keys [neighbors-fn] :or {neighbors-fn u/neighbors4}}]
  (let [rows (->> txt
                  clojure.string/split-lines
                  (mapv #(u/read-as-vector (clojure.string/join " " %))))
        w (count (first rows))
        h (count rows)]
    (entries->graph (into [] (apply concat rows)) w h :neighbors-fn neighbors-fn)))

(defn recover-first-path [{:keys [spt dist]} from to]
  (let [xs (->> to
                (iterate (fn [nd] (let [pred (spt nd)]
                                    (if (coll? pred)
                                      (first pred)
                                      pred))))
                (take-while #(not= % from))
                reverse
                (into [from]))]
    ;;need to remove the starting position cost
    {:path xs :length (- (dist to) (dist from))}))

;;day 15.1
(let [gr (-> (io/resource "day15input.txt")
             slurp
             txt->graph)
      destination (reduce max (keys  (gr :nodes)))]
  (-> gr
      (best-first 0 destination)
      (recover-first-path 0 destination)))

;;day 15.2

;;Space is now a concern.  We may shift to an implicit graph and
;;compute our weights.

;;we can compute the weights for our new graph tiles.

;;we have entries, in a 1kx1k grid.

#_
(defn flyweight-graph [txt r c]
  (let [{:keys [width height nodes] :as base} (txt->graph txt)
        n-base (count nodes)]
    {:base base
     :nodes (fn [entry] )
     :sinks (fn [entry] )}))
