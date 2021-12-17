(ns aoc2021.day17
  (:require
     [clojure.java.io :as io]
     [aoc2021.util :as u]))


(def sample
  "target area: x=20..30, y=-10..-5")

;;ugh, first thought is to look up analytic
;;solution by revisiting physics, then solving, blah.

;;Dumb option is to use numerical solution and search.
;;Let's search...

(defn unparse [eq]
  (-> eq
      (clojure.string/replace  #"[xy=]" {"x" ":x " "y" ":y " "=" "["})
      (clojure.string/replace ".." " ")
      (str "]")))

(defn txt->target [txt]
  (let [bnd (->> (clojure.string/split sample #",")
                 (map #(unparse (re-find #"[xy]=.+" %)))
                 (clojure.string/join " ")
                 u/braces
                 clojure.edn/read-string)
        [xmin xmax] (bnd :x)
        [ymin ymax] (bnd :y)]
    {:xmin xmin :xmax xmax
     :ymin ymin :ymax ymax}))


;;naive.  a better man would leverag projectile physics.  I failed.
(defn stepper [n vx vy]
  (->> (iterate (fn [[x y vx vy]]
                  [(+ x vx) (+ y vy) (- vx 1) (- vy 1)])
                [0 0 vx vy])
       (map (fn [v] [(v 0) (v 1)]))
       (take (inc n))))

;;we know some stuff.
;;we can shoot with as much v as we want, except
;;there are tradeoffs.
;;Naive method is to solve for x independently of y.
;;If we know the bounds on x, then we can bound y.
;;we can naively guess our vx and binary search to see if we ever hit the bounds.
;;We also know the bounds of the target, so we can compute
;;containment for any step efficiently.

;;if x1 = x2, we have hit our rightmost point.

;;what is the min xvelocity that gets us to the left of the bounds?

(defn classify [{:keys [xmin xmax ymin ymax]} x y]
  (cond
    (< x xmin) ;;left of target.
       (if (> y ymax)   ;;above
         :left-up
         (if (>= y ymin) ;;centered.
           :left-center
           :left-below))
    (<= x xmax)  ;;over target.
        (if (> y ymax)   ;;too high
          :center-up
          (if (>= y ymin)
            :center-center ;;on target.
            :center-below)) ;;too low
        :else :right)) ;;overshot, no possibility of return.

(defn feasible? [bounds x y]
  (case (classify bounds x y)
    (:left-up :left-center :center-up :center-center) true
    false))

(defn idx-bounds [bounds x y]
  (case (classify bounds x y)
    :left-up  \\
    :center-up \v
    :right     \<
    :center-center \O
    :center-below \^
    :left-below \/
    :left-center \<))

(defn cart-grid [width ymin ymax f]
  (->> (for [y (range ymin ymax)]
         (if (= y 0)
           (map (fn [x] (f x y)) (range width))
           (mapv (fn [x] (f x y)) (range width))))

       reverse))

(defn feas-grid [bounds width ymin ymax]
  (doseq [xs (cart-grid width ymin ymax
                        (fn [x y] (idx-bounds bounds x y)))]
    (println xs)))

#_
(feas-grid sbounds 35  -20 20)
