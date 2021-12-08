(ns aoc2021.day7
  (:require [clojure.java.io :as io]
            [aoc2021.util :as u]))

(def sample [16,1,2,0,4,2,7,1,2,14])

(defn score [xs pos]
   (->> xs
        (map (fn [x]
               (Math/abs (- x pos))))
        (reduce +)))

;;our goal is pretty simple.
;;I think we assume that this is a parabola, even the integer constrained one.
;;The continuous relaxation is definitely a parabola.

;;I think we take an initial guess, based on the optimum in contiuous
;;space (average weight).  Sample that on either side, determine
;;which side of the saddle we are on.

;;we sample the current point (m), 1 left, 1 right.
;;If m is the saddle, done.  If we're left of the saddle, we need to guess another point.

;;we can provide a bounded guess if we know something of the curvature of the left side of the saddle.

;;Any time we guess a point, we collect samples.  These samples provide tangent lines
;;of the response surface.  If we have a left bound and a right bound, then we
;;can maintain linear bounds on the left and right and use the intersection of these
;;lines to provide a guess.  Since we are bounding a parabola, these lines should follow
;;the contour and direct us "toward" the minimum.  we may over/undershoot, but we
;;can update our bounds based on the new information.

;;so we maintain some state
;;{:left-bound  [slope intercept]
;; :right-bound [slope intercept]}

;;given the bounds we can compute an intersection and a guess.
;;we compute the intersection and sample it.
;;if it's not a saddle, we take the slope of the "inner" most
;;point depending on the saddle.  We also want to maintain
;;the least slope on either side (least magnitude).

;;from here we can binary search through candidates, I think.

(defn saddle-class [l m r]
  (cond (and (< m l)
             (< m r))
        :saddle
        (< l m r)
        :right
        (> l m r)
        :left))

;;y = lm(x) + lb
;;y = rm(x) + rb
;;lm(x) + lb = rm(x) + rb
;;lb - rb = rm(x) - lm(x)
;;lb - rb = (rm-lm)x
;;(lb - rb)/(rm - lm) = x0
;;y = lm(x') + lb
;;don't really need y, but meh.
(defn intersect [[lm lb] [rm rb]]
  (let [num (- lb rb)
        den (- rm lm)
        x0  (double (/ num den))
        y0  (+ lb (* lm x0))]
    [x0 y0]))

;;y = mx + b
;;b = y - mx
(defn compute-line [x1 y1 x2 y2]
  (let [m (double (/ (- y2 y1) (- x2 x1)))
        b (- y1 (* m x1))]
    [m b]))

;;given bounds, estimate a point based on the
;;intersection of 2 lines.
(defn estimate-point [[l r]]
  (let [[x y] (intersect l r)]
    x))

;;given a point, generate a result, either
;;a {:saddle pt}, {:left [m b]}, {:right [m b]}
(defn sample-point [f x]
  (let [l  (dec x)
        m  x
        r  (inc x)
        fl (f l)
        fm (f m)
        fr (f r)
        position (saddle-class fl fm fr)
        _ (u/log [[l fl] [m fm] [r fr] position])]
    (case  position
      :saddle [:saddle [x fm]]
      :left   [:left  (compute-line m fm r fr)]
      :right  [:right (compute-line l fl m fm)])))

;;need an initial guess on left bound and right bound.
;;assume we have initial bounds.

;;we estim ate a point.
;;that point is either left/saddle/right.
;;based on the point, we update our bounds.
;;if left, we have new left bounds.
;;if right, we have new right bounds.
(defn saddle-step [f l r]
  (let [[x0 _]       (intersect    l r) ;;our projected guess
        x         (Math/round x0)
        _ (u/log [:guessing x])
        [type res]  (sample-point f x)] ;;either left, saddle, right.
    (case type
      :saddle {:saddle res} ;;found saddle point, done.
      :left   {:new-bounds [res r]} ;;update left bounds
      :right  {:new-bounds [l res]};;update right bounds
      )))

(defn saddle-search [f xl xr]
  (let [[typel l] (sample-point f xl)
        [typer r] (sample-point f xr)]
    (cond (= typel :saddle) l ;;done
          (= typer :saddle) r ;;done
          :else ;;go search
          (do (assert (and (= typel :left) (= typer :right))
                      "expected initial xl, xr to define bounds!")
              (->> (iterate (fn [{:keys [l r] :as state}]
                         (let [res (saddle-step f l r)]
                           (if (res :saddle) ;;done
                             (assoc state :saddle (res :saddle))
                             (let [[l r] (res :new-bounds)]
                               {:l l :r r}))))
                            {:l l :r r})
                   (drop-while #(not (% :saddle)))
                   first)))))

(defn bounds [xs]
  (reduce (fn [[xmin xmax] x]
            [(min xmin x) (max xmax x)])
          [Long/MAX_VALUE Long/MIN_VALUE] xs))

;;10x faster for this data than exhaustive.
(defn solve [xs & {:keys [f] :or {f score}}]
  (let [[xmin xmax] (bounds xs)
        score-fn    (u/memo-1 #(f xs %))]
    (-> (saddle-search score-fn xmin xmax)
        :saddle)))

;;exhaustive check
(defn exhaustive [xs & {:keys [f] :or {f score}}]
  (let [[xmin xmax] (bounds xs)
        score-fn    #(f xs %)]
    (reduce (fn [[x0 y] n]
              (let [res (score-fn n)]
                (if (< res y) ;;new min
                  [n res]
                  (reduced [x0 y]))))
            [(dec xmin) (score-fn (dec xmin))] ;;increases not allowed!
            (range xmin (inc xmax)))))

;;solve day 7.1
(->> (io/resource "day7input.txt")
     slurp
     u/brackets
     clojure.edn/read-string
     solve
     second)

;;solve 7.2

#_
(defn naive-cost [n]
  (reduce + (take n (range 1 (inc n)))))

#_
(def costs
  (let [n   100
        acc (long-array (range 100))]
    (loop [idx 0
           sum 0]
      (when (< idx n)
        (let [nidx    (unchecked-inc idx)
              next-sum (+ sum idx)]
          (aset acc idx next-sum)
          (recur nidx next-sum))))
    acc))

;;we can cache these.  can we compute?

(defn gauss-cost [n]
  (/ (* n (inc n)) 2))

(defn new-score [xs pos]
  (->> xs
       (map (fn [x]
              (gauss-cost (Math/abs (- x pos)))))
       (reduce +)))

;;our response is still parabolic, so assumptions hold.

(-> (io/resource "day7input.txt")
     slurp
     u/brackets
     clojure.edn/read-string
     (solve :f new-score)
     second)

;;same 
#_
(-> (io/resource "day7input.txt")
    slurp
    u/brackets
    clojure.edn/read-string
    (exhaustive :f new-score)
    second)
