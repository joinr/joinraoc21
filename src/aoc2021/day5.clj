(ns aoc2021.day5
  (:require [clojure.java.io :as io]))

(def sample
  "0,9 -> 5,9
   8,0 -> 0,8
   9,4 -> 3,4
   2,2 -> 2,1
   7,0 -> 7,4
   6,4 -> 2,0
   0,9 -> 2,9
   3,4 -> 1,4
   0,0 -> 8,8
   5,5 -> 8,2")

(defn txt->segs [txt]
  (->> (for [ln (clojure.string/split-lines txt)]
         (let [xs (clojure.string/split ln #"->")]
           (mapv #(clojure.edn/read-string (str "[" % "]")) xs)))
       (mapv (fn [[l r]]
              (if (> (l 0) (r 0)) ;;get into ordered format, since direction doesn't matter.
                [r l]
                [l r])))))

(def xs (txt->segs sample))
;;separating axis theorem should let us prune intersections...

;;assumes all y are equal...
(defn hsect [l r]
  (let [[[lx1 y] [lx2 _]] l
        [[rx1 _] [rx2 _]] r]
    (if (> lx1 rx1)
      (recur r l)
      (when-not (< lx2 rx1) ;;----- [    ]
        (let [left-overlap  (and (<= rx1 lx2) (>= rx1 lx1)) ;;---[----
              right-overlap (and (<= rx2 lx2) (>= rx2 lx1))] ;;-----]--
          (cond
            (and left-overlap right-overlap) ;;     ---[--]---
            r ;;overlapping segment.
            left-overlap                     ;;     -----[----   ]
            [[rx1 y] [lx2 y]]
            right-overlap                    ;;[    -----]----
            [[lx1 y] [rx2 y]]
            :else
            (when (and (<= rx1 lx1) (>= rx2 lx2)) l) ;;[    -----    ]
            ))))))

(defn vsect [l r]
  (let [[[x ly1] [_ ly2]] l
        [[_ ry1] [_ ry2]] r]
    (if (> ly1 ry1)  (recur r l)
        (when-not (<= ly2 ry1) ;;----- [    ]
          (let [left-overlap  (and (<= ry1 ly2) (>= ry1 ly1)) ;;---[----
                right-overlap (and (<= ry2 ly2) (>= ry2 ly1))] ;;-----]--
            (cond
              (and left-overlap right-overlap) ;;     ---[--]---
              r ;;overlapping segment.
              left-overlap                     ;;     -----[----   ]
              [[x ry1] [x ly2]]
              right-overlap                    ;;[    -----]----
              [[x ly1] [x ry2]]
              :else
              (when (and (<= ry1 ly1) (>= ry2 ly2)) l) ;;[    -----    ]
              ))))))

(defn intersect2d [l r]
  (let [h  (hsect l r)
        v  (vsect l r)]
    (cond (and h v)
          (let [pt [(ffirst h) (second (first  v))]]
            [pt pt])
          (and h (= (second (first l)) (second (first r))))
            h
          (and v (= (first (first l)) (first (first r))))
            v
          :else nil)))

;;brute force ->
;;compare each line to all others for intersection.
;;better -> do a plane sweep over the segments sorted by x coordinate.
;;maintain an open set of segments intersecting the sweep line.
;;determine intersections among these segments.
;;advance the search by moving to the next x coordinate; remove lines that
;;no longer intersect from the set.
;;keep track of intersecting point density.

(defn drop-segments [{:keys [actives pending intersections x history] :as acc}]
  (let [[k removals]      (first pending)
        _ (println [:dropping-segments k removals])
        new-actives       (reduce dissoc actives removals)
        new-pending       (dissoc pending k)
        new-intersections (reduce-kv (fn [acc point xs]
                                       (let [res (clojure.set/difference xs removals)]
                                         (if (> (count res) 1)
                                           (assoc acc point res)
                                           acc)))
                                     {} intersections)]
    (assoc acc
           :actives new-actives
           :pending new-pending
           :intersections new-intersections
           :history (conj history [k intersections]))))

(defn log [x]
  (do (println x)
      x))

(defn intersections [xs]
  (let [lines (group-by (fn [[l r]]
                          (cond (= (l 0) (r 0)) :horizontal
                                (= (l 1) (r 1)) :vertical
                                :else  :diagonal)) xs)
        sorted (sort-by ffirst (vec (concat (lines :horizontal) (lines :vertical))))
        lc     (atom 0)]
    ;;actives :: map of {l1 seg} ;;active lines
    ;;pending :: sorted-map of xf -> #{l1 l2 l3} ;;rightmost points of lines.
    ;;intersesctions :: map of {0 #{l1 l2} etc.
    ;;x :: expected x coordinate.}
    (loop [{:keys [actives pending intersections history x] :as acc}
           {:actives       {}
            :pending       (sorted-map-by <)
            :intersections {}
            :x Long/MIN_VALUE
            :history []}
           segs   sorted]
          ;;we are in-bounds!
          ;;add the segment to the working set.
      (if-let [[l r :as seg] (first segs)]
        (let [xnew (first l)
              _ (println seg)]
          (if (some-> pending keys first (< xnew))
            (recur (log (drop-segments  (assoc acc :x xnew))) segs)
            (let [[x0 _]    l
                  [x1 _]    r ;;terminus
                  ;;label and add line.
                  id      (swap! lc inc)
                  ;;update end points with new line.
                  pending (update pending x1 (fn [coll] (conj (or coll #{}) id)))
                  ;;determine intersections.
                  added-intersections (reduce-kv (fn [acc k other]
                                                   (if-let [res (intersect2d seg other)]
                                                     (update acc res #(conj (or % #{id}) k))
                                                     acc)) {} actives)
                  actives (assoc actives id seg)]
              (recur (log (assoc acc :actives actives :pending pending
                                 :intersections (merge-with clojure.set/union intersections added-intersections)
                                 :x x0))
                     (rest segs)))))
          acc))))

