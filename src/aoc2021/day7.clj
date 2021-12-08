(ns aoc2021.day7
  (:require [clojure.java.io :as io]))

(def sample [16,1,2,0,4,2,7,1,2,14])

(defn score [xs pos]
   (->> xs
        (map (fn [x]
               (Math/abs (- x pos))))
        (reduce +)))

;;dumbest idea is to binary search.
;;only works if we have a global optimum.

;;assume picking a position larger than all the others
;;provides an upper bound, since all deviations will be
;;positive.

;;looks like this forms a parabola, with the global minimum being the
;;average of the weights.  Obviously this is in continuous space,
;;we have to operate in integer space.

;;We can probably leverage the continuous relaxation to
;;get a good initial guess.  Then maybe search from there.

(defn init-guess [xs]
  (Math/round
   (/ (reduce + xs)
      10.0)))


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
{:left-bound  [slope intercept]
 :right-bound [slope intercept]}

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

;;given a sample, update the bounds.
(defn update-bounds [l r position m]
  )

;;given bounds, estimate a point based on the
;;intersection of 2 lines.
(defn estimate-point [[l r]]
  (let [[x y] (intersect l r)]
    x))

;;given a point, generate a result, either
;;a {:saddle pt}, {:left [m b]}, {:right [m b]}
(defn sample-point [f x]
  (let [l (dec x)
        m x
        r (inc x)
        fl (f l)
        fm (f m)
        fr (f r)
        position (saddle-class fl fm fr)
        _ (println [[l fl] [m fm] [r fr] position])]
    (case  position
      :saddle {:saddle [x fm]}
      :left   {:left  (compute-line m fm r fr)}
      :right  {:right (compute-line l fl m fm)})))

;;need an initial guess on left bound and right bound.
;;assume we have initial bounds.
#_
(defn saddle-step [f xl xr]
  (let [left-bound  (sample-point f xl)
        right-bound (sample-point f xr)]
    (if-let [saddle (or (left-bound :saddle) (right-bound :saddle))]
      saddle ;;done
      ;;it's possible either bound overshoots. what then?
      
      )))



;;can hill climb.
