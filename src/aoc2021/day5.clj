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

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;;from christophe
;;http://clj-me.cgrand.net/2009/10/15/multidim-arrays/
(defmacro deep-aget
  ([hint array idx]
   `(aget ~(vary-meta array assoc :tag hint) ~idx))
  ([hint array idx & idxs]
   `(let [a# (aget ~(vary-meta array assoc :tag 'objects) ~idx)]
      (deep-aget ~hint a# ~@idxs))))

(defmacro deep-aset [hint array & idxsv]
  (let [hints '{doubles double ints int} ; writing a comprehensive map is left as an exercise to the reader
        [v idx & sxdi] (reverse idxsv)
        idxs (reverse sxdi)
        v (if-let [h (hints hint)] (list h v) v)
        nested-array (if (seq idxs)
                       `(deep-aget ~'objects ~array ~@idxs)
                       array)
        a-sym (with-meta (gensym "a") {:tag hint})]
    `(let [~a-sym ~nested-array]
       (aset ~a-sym ~idx ~v))))

(defn compute-bounds [xs]
  (reduce (fn [{:keys [xmin xmax ymin ymax]} [[x1 y1] [x2 y2]]]
            {:xmin (min xmin x1 x2)
             :xmax (max xmax x1 x2)
             :ymin (min ymin y1 y2)
             :ymax (max ymax y1 y2)})
          {:xmin Long/MAX_VALUE :xmax Long/MIN_VALUE
           :ymin Long/MAX_VALUE :ymax Long/MIN_VALUE}
          xs))

;;brute force 
(defn dumbass [xs]
  (let [lines  (filtered-lines xs)
        bounds (compute-bounds lines)
        w (inc (- (bounds :xmax) (bounds :xmin)))
        h (inc (- (bounds :ymax) (bounds :ymin)))
        ^long
        xoffset (bounds :xmin)
        ^long
        yoffset (bounds :ymin)
        grid (make-array Long/TYPE w h)
        fill (fn [^long x1 ^long y1 ^long x2 ^long y2]
               (if (= x1 x2)
                 (doseq [y (range (min y1 y2) (inc (max y1 y2)))]
                   (deep-aset longs grid x1 y (inc ^long (deep-aget longs grid x1 y))))
                 (doseq [x (range (min x1 x2) (inc (max x1 x2)))]
                   (deep-aset longs grid x y1 (inc ^long (deep-aget longs grid x y1))))))
        adjust (fn [[[^long x1 ^long y1] [^long x2 ^long y2]]]
                 [(- x1 xoffset) (- y1 yoffset) (- x2 xoffset) (- y2 yoffset)])
        n      (atom [])]
    (doseq [l lines]
      (let [[x1 y1 x2 y2] (adjust l)]
        (try (fill x1 y1 x2 y2)
             (catch Exception e (throw (ex-info "bad-fill" {:in [x1 y1 x2 y2]}))))))
    ;;scan the array.
    (doseq [i (range w)
            j (range h)]
      (let [v (deep-aget longs grid i j)]
        (when (> v 1)
          (swap! n conj {:x (+ i xoffset) :y (+ j yoffset) :n v}))))
    @n))

;;4873

;;day 5.1 solution

(->> (io/resource "day5input.txt")
     slurp
     txt->segs
     dumbass #_intersecting-points
     (filter #(>= (% :n) 2))
     count)

;;5.2 generalizes to add diagonal
(defn dumbass2 [xs]
  (let [bounds (compute-bounds xs)
        lines  (sort-by ffirst xs) ;;sorted by x1.
        w (inc (- (bounds :xmax) (bounds :xmin)))
        h (inc (- (bounds :ymax) (bounds :ymin)))
        ^long xoffset (bounds :xmin)
        ^long yoffset (bounds :ymin)
        grid (make-array Long/TYPE w h)
        fill (fn [^long x1 ^long y1 ^long x2 ^long y2]
               (cond (= x1 x2)
                     (doseq [y (range (min y1 y2) (inc (max y1 y2)))]
                       (deep-aset longs grid x1 y (inc ^long (deep-aget longs grid x1 y))))
                     (= y1 y2)
                     (doseq [x (range (min x1 x2) (inc (max x1 x2)))]
                       (deep-aset longs grid x y1 (inc ^long (deep-aget longs grid x y1))))
                     :else
                     (let [slope (if (> y2 y1) 1 -1)
                           delta  (- x2 x1)]
                       (dotimes [step (inc delta)]
                         (let [x (+ step x1)
                               y (+ (* slope step) y1)]
                           (deep-aset longs grid x y (inc ^long (deep-aget longs grid x y))))))))
        adjust (fn [[[^long x1 ^long y1] [^long x2 ^long y2]]]
                 [(- x1 xoffset) (- y1 yoffset) (- x2 xoffset) (- y2 yoffset)])
        n      (atom [])]
    (doseq [l lines]
      (let [[x1 y1 x2 y2] (adjust l)]
        (try (fill x1 y1 x2 y2)
             (catch Exception e (throw (ex-info "bad-fill" {:in [x1 y1 x2 y2]}))))))
    ;;scan the array.
    (doseq [i (range w)
            j (range h)]
      (let [v (deep-aget longs grid i j)]
        (when (> v 1)
          (swap! n conj {:x (+ i xoffset) :y (+ j yoffset) :n v}))))
    @n))

;;refactored dumbass2, could be golfed much further.
(defn dumbass3 [xs]
  (let [bounds (compute-bounds xs)
        lines  (sort-by ffirst xs) ;;sorted by x1.
        w (inc (- (bounds :xmax) (bounds :xmin)))
        h (inc (- (bounds :ymax) (bounds :ymin)))
        ^long xoffset (bounds :xmin)
        ^long yoffset (bounds :ymin)
        grid (make-array Long/TYPE w h)
        bump! (fn [x y] (deep-aset longs grid x y (inc ^long (deep-aget longs grid x y))))
        fill (fn [^long x1 ^long y1 ^long x2 ^long y2]
               (let [dx     (- x2 x1)  ;;always positive
                     dy     (- y2 y1)
                     slope  (when (pos? dx) (/ dy dx))
                     f      (if (pos? dy) + -)]
                 (if slope ;;horizontal or diagonal.
                   (dotimes [step  (inc dx)]
                     (bump! (+ step x1) (+ (* slope step) y1)))
                   (dotimes [step (inc (f dy))] ;;vertical
                     (bump! x1 (f y1 step))))))
        adjust (fn [[[^long x1 ^long y1] [^long x2 ^long y2]]]
                 [(- x1 xoffset) (- y1 yoffset) (- x2 xoffset) (- y2 yoffset)])
        n      (atom [])]
    (doseq [l lines]
      (let [[x1 y1 x2 y2] (adjust l)]
        (try (fill x1 y1 x2 y2)
             (catch Exception e (throw (ex-info "bad-fill" {:in [x1 y1 x2 y2]}))))))
    ;;scan the array.
    (doseq [i (range w)
            j (range h)]
      (let [v (deep-aget longs grid i j)]
        (when (> v 1)
          (swap! n conj {:x (+ i xoffset) :y (+ j yoffset) :n v}))))
    @n))

(->> (io/resource "day5input.txt")
     slurp
     txt->segs
     dumbass2 #_intersecting-points
     (filter #(>= (% :n) 2))
     count)


;;OBE
;;Broken plane-sweep algo.  Revisit.

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
            (if (not= lx1 lx2 rx1 rx2)
              r ;;overlapping segment.
              :vertical)
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
        (when-not (< ly2 ry1) ;;----- [    ]
          (let [left-overlap  (and (<= ry1 ly2) (>= ry1 ly1)) ;;---[----
                right-overlap (and (<= ry2 ly2) (>= ry2 ly1))] ;;-----]--
            (cond
              (and left-overlap right-overlap) ;;     ---[--]---
              (if (not= ly1 ly2 ry1 ry2)
                r ;;overlapping segment.
                :horizontal)
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
          (cond (and (vector? h) (vector? v))
                  (let [pt [(ffirst h) (second (first  v))]]
                    [pt pt])
                  (= v :horizontal)
                  h
                  (= h :vertical)
                  v)
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

(def ^:dynamic *logging* nil)
(defn log [x]
  (when *logging*
    (println x))
  x)

(defn drop-segments [{:keys [actives pending intersections history] :as acc}]
  (let [[k removals]      (first pending)
        _ (log [:dropping-segments k removals])
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
           :xprev   k
           :history (conj history {:x k :intersections intersections :actives actives}))))

(defn filtered-lines [xs]
  (let [lines (group-by (fn [[l r]]
                    (cond (= (l 0) (r 0)) :horizontal
                          (= (l 1) (r 1)) :vertical
                          :else  :diagonal)) xs)]
    (sort-by ffirst (vec (concat (lines :horizontal) (lines :vertical))))))

(defn intersections [xs]
  (let [sorted (filtered-lines xs)
        [[xinit _] _]   (first sorted)
        lc     (atom 0)]
    ;;actives :: map of {l1 seg} ;;active lines
    ;;pending :: sorted-map of xf -> #{l1 l2 l3} ;;rightmost points of lines.
    ;;intersesctions :: map of {0 #{l1 l2} etc.
    ;;x :: expected x coordinate.}
    (loop [{:keys [actives pending intersections history x xprev] :as acc}
           {:actives       {}
            :pending       (sorted-map-by < xinit nil)
            :intersections {}
            :x     xinit
            :xprev xinit
            :history []}
           segs   sorted]
          ;;we are in-bounds!
      ;;add the segment to the working set.
      ;;if the next segment causess an advance, we commit history.
      ;;next segment causes us to advance if it is > xprev, or > min of pending.
      (if-let [[l r :as seg] (first segs)]
        (let [[x0 _] l
              _ (log seg)]
          (if (some-> pending keys first (< x0))
            (recur (log (drop-segments  (assoc acc :x x0))) segs)
            (let [[x1 _]    r ;;terminus
                  xchanged?           (not= x0 xprev)
                  no-prior-history?   (not= (some-> history peek :x) xprev)
                  _ (log [xchanged? no-prior-history? xprev x0])
                  new-history (when (and xchanged? no-prior-history?)
                                (conj history {:x xprev :intersections intersections :actives actives}))
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
                                 :x x0
                                 :xprev x
                                 :history (or new-history history))) (rest segs)))))
        ;;no more segments, try to drain the pending!
        (if-let [xnew (some-> pending keys first inc)]
          (recur (log (drop-segments  (assoc acc :x xnew))) segs)
          (acc :history))))))


(defn intersecting-points [segs]
  (for [{:keys [x intersections]} (intersections segs)
        [[l r] members] intersections]
    {:x x :y (second l) :n (count members)}))
