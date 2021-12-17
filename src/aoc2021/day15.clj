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

(defn txt->rows [txt]
  (->> txt
       clojure.string/split-lines
       (mapv #(u/read-as-vector (clojure.string/join " " %)))))

#_#_
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

(defn relax
  ([{:keys [spt dist fringe] :as state} source sink w hw]
   (let [dfrom (or (dist source) (throw (ex-info "unknown weight!" {:in dist :source source})))
         wnew  (+ ^long dfrom ^long w)]
     (if-let [^long wold (dist sink)] ;;known path.
       (cond (< wnew wold) ;;shorter path
             {:spt    (assoc spt sink source)
              :dist   (assoc dist sink wnew)
              :fringe (u/push-fringe fringe (+  wnew ^long hw) sink)}
             (= wnew wold) ;;equal path
             {:spt  (update spt sink multipath source)
              :dist dist
              :fringe fringe}
             :else ;;no change
             state)
       ;;new path
       {:spt    (assoc spt  sink source)
        :dist   (assoc dist sink wnew)
        :fringe (u/push-fringe fringe (+ wnew ^long hw) sink)})))
  ([state source sink w] (relax state source sink w 0)))

(defn best-first [gr from to & {:keys [make-fringe on-visit]
                                :or {make-fringe u/min-pq
                                     on-visit identity}}]
  (let [sinks (gr :sinks)
        nodes (gr :nodes)
        weightf (fn [nd]
                  ((nodes nd) :risk))
        init {:spt  {from from}
              :dist {from (weightf from)}
              :fringe (make-fringe [0 from])}]
    (loop [{:keys [spt dist fringe] :as state} init]
      (if-let [source (u/peek-fringe fringe)]
        (if (= source to)
          (assoc state :found-path [from to])
          (let [_ (on-visit source)
                neighbors  (sinks source)
                next-state (reduce-kv (fn [acc sink w]
                                        (relax acc source sink w))
                                      (update state :fringe u/pop-fringe)
                                      (sinks source))]
            (recur next-state)))
        state))))

(defn a* [gr from to h & {:keys [make-fringe on-visit]
                          :or {make-fringe u/min-pq
                               on-visit identity}}]
  (let [sinks (gr :sinks)
        nodes (gr :nodes)
        weightf (fn [nd]
                  ((nodes nd) :risk))
        init {:spt  {from from}
              :dist {from (weightf from)}
              :fringe (make-fringe [0 from])}]
    (loop [{:keys [spt dist fringe] :as state} init]
      (if-let [source (u/peek-fringe fringe)]
        (if (= source to)
          (assoc state :found-path [from to])
          (let [_ (on-visit source)
                neighbors  (sinks source)
                next-state (reduce-kv (fn [acc sink w]
                                        (let [hw (h sink to)]
                                          (relax acc source sink w hw)))
                                      (update state :fringe u/pop-fringe)
                                      (sinks source))]
            (recur next-state)))
        state))))

(defn manhattan [w h from to]
  (let [xy1 (u/idx->xy w h from)
        xy2 (u/idx->xy w h to)]
    (+ (Math/abs (- ^long (xy2 0) ^long (xy1 0)))
       (Math/abs (- ^long (xy2 1) ^long (xy1 1))))))

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
  (let [rows (txt->rows txt)
        w (count (first rows))
        h (count rows)]
    (entries->graph (into [] (apply concat rows)) w h :neighbors-fn neighbors-fn)))

(defn recover-first-path
  ([{:keys [spt dist]} from to]
   (let [xs (->> to
                 (iterate (fn [nd] (let [pred (spt nd)]
                                     (if (coll? pred)
                                       (first pred)
                                       pred))))
                 (take-while #(not= % from))
                 reverse
                 (into [from]))]
     ;;need to remove the starting position cost
     {:path xs :length (- ^long (dist to) ^long (dist from))}))
  ([{:keys [found-path] :as state}]
   (when-let [[from to] found-path]
     (recover-first-path state from to))))

;;day 15.1
#_
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

;;given a [w h] [r c],
;;we want a function that maps  [[0 ... 5x] [0 ... 5x]] to [x' y']

(defn xy->xyuw [^long x ^long y ^long w ^long h]
  (let [u (quot x w)
        x (rem  x w)
        w (quot y h)
        y (rem y h)]
    [x y u w]))

(defn tile
  ([rows w h c r xform]
   (let [xmax (long (* ^long w ^long c))
         ymax (long (* ^long h ^long r))
         w (long w)
         h (long h)]
     (fn [^long x ^long y]
       (when (and (> x -1) (< x xmax)
                  (> y -1) (< y ymax))
         (let [u (quot x w)
               x0 (rem  x w)
               w (quot y h)
               y0 (rem y h)
               v  ((rows y0) x0)]
           (xform x0 y0 u w v))))))
  ([rows w h c r] (tile rows w h c r (fn [_ _ _ _ v] v))))

(defn wrapping-add [^long v ^long n]
  (let [res (unchecked-add v n)]
    (if (< res 10)
      res
      (let [n (rem res 9)]
        (if-not (zero? n)
          n
          9)))))

(defn wrapping-tile [rows w h c r]
  (tile rows w h c r (fn [x0 y0 u w v]
                       (wrapping-add v (+ ^long u ^long w)))))

(defn txt->tiled-graph [txt ^long r ^long c]
  (let [rows (txt->rows txt)
        w (count (first rows))
        h (count rows)
        tw (* w c)
        th (* h r)
        tiled-entries (wrapping-tile rows w h r c)
        tiled-sinks   (fn [nd]
                        (let [xy (u/idx->xy tw th nd)]
                          (->> (for [point (u/neighbors4 (xy 0) (xy 1))
                                     :let [v (tiled-entries (point 0) (point 1))]
                                     :when v]
                                 [(u/xy->idx tw th (point 0) (point 1)) v])
                               (into {}))))
        tiled-nodes (fn [nd]
                      (let [xy (u/idx->xy tw th nd)]
                        {:node nd :risk (tiled-entries (xy 0) (xy 1)) :coord xy}))]
    {:base-width w
     :base-height h
     :width  tw
     :height th
     :cols   c
     :rows   r
     :nodes  tiled-nodes
     :sinks  tiled-sinks
     :entries tiled-entries}))

(defn tiled-graph-entries [{:keys [width height cols rows nodes sinks entries]}]
  (vec (for [y (range height)]
         (mapv (fn [x] (entries x y)) (range width)))))

(defn print-tiled-graph [{:keys [width height cols rows nodes sinks entries]}]
  (doseq [y (range height)]
    (println (mapv (fn [x] (entries x y)) (range width)))))

(let [l (first known) r (first  (tiled-graph-entries tg))]
  (->> (map vector l r)
       (map-indexed vector)
       (some (fn [[x [l r] :as s]] (and (not= l r) s)))))

(defn highlight-path [{:keys [width height cols rows nodes sinks entries] :as gr} path]
  (let [ents (tiled-graph-entries gr)]
    (reduce (fn [acc nd]
              (let [[x y] (u/idx->xy width height nd)]
                (assoc-in acc [y x] \* )))
            ents path)))

;;day 15.1
#_
(let [{:keys [width height] :as gr}
         (-> (io/resource "day15input.txt")
             slurp
             (txt->tiled-graph 5 5))
      destination (dec (* width height))]
  (-> gr
      (best-first 0 destination)
      (recover-first-path 0 destination)))

;;ugh, completes in 2s.  Should use A* to prune better,
;;or an indexed pq.  Could also optimize the representation
;;using primitive arithmetic.

;;indexed priority queue isn't a big help here, at least the non-optimized
;;implementation.  very curious why a* isn't pruning more.
