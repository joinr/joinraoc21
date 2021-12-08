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

;;from here we can binary search through candidates, I think.

(defn saddle-class [l m r]
  (cond (and (< m l)
             (< m r))
        :saddle
        (< l m r)
        :right
        (> l m r)
        :left))

(defn saddle-search [f l r]
  (let [dist   (- r l)
        m      (+ l (quot dist 2))
        sclass (saddle-class (f l) (f m) (f r))
        _ (println [sclass l m r])]
    (case sclass
      :saddle m
      :left   (recur f r (+ r dist))
      :right  (recur f (- l dist)  l))))

;;can hill climb.

