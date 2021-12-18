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
  (let [bnd (->> (clojure.string/split txt #",")
                 (map #(unparse (re-find #"[xy]=.+" %)))
                 (clojure.string/join " ")
                 u/braces
                 clojure.edn/read-string)
        [xmin xmax] (bnd :x)
        [ymin ymax] (bnd :y)]
    {:xmin xmin :xmax xmax
     :ymin ymin :ymax ymax}))


;;naive.  a better man would leverag projectile physics.  I failed.
(defn stepper
  ([vx vy]
   (->> (iterate (fn [[x y vx vy]]
                   [(+ x vx) (+ y vy) (if (zero? vx) vx (- vx 1)) (- vy 1)])
                 [0 0 vx vy])
        (map (fn [v] [(v 0) (v 1)]))))
  ([n vx vy] (->> (stepper vx vy)
                  (take (inc n)))))

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

;;our criteria, given a sequence of points, is to check if the
;;points are either in the center of the bounds, or if the are
;;left-below, center-below, or right.

(defn halting-state [state] (#{:left-below :center-center :right :center-below} state))

(defn fire [bounds vx vy]
  (let [n (atom 0)
        prev (atom Long/MIN_VALUE)
        {:keys [xmin]} bounds
        hmax (atom 0)]
    (->> (stepper vx vy)
         (map-indexed (fn [idx [x y]]
                        (swap! hmax max y)
                        {:state (classify bounds x y)
                         :t idx
                         :coord [x y]}))
         (some (fn [st]
                 (let [x   ((st :coord) 0)
                       res (if-let [state (halting-state (st :state))]
                             (assoc st :halted state)
                             (if (and (= @prev x)
                                      (< x xmin))
                               (assoc st :halted :stalled-left)
                               #_(when (> (swap! n inc) 200)
                                       (throw (ex-info "blew-up" {:in st})))))
                       _   (reset! prev x)]
                   (when res (assoc res :ymax @hmax))))))))


;;max horizontal distance we can travel...equivalent to reverse sum of
;;vx over time.
(defn max-x-displacement [vx]
  (/ (* vx (+ vx 1)) 2))


;;probably onto something here...screw it though.
(defn horizontal-guess [vx]
  [(max-x-displacement vx) (- (max-x-displacement (dec vx)))])

(defn guess-right [xmin xmax]
  (->> (range 1 xmin)
       (some (fn [x]
               (let [xd (max-x-displacement x)]
                 (when (and (>= xd xmin) (<= xd xmax))
                   x))))))

;;solve 17.1

;;just realized the critieria indicates the largest y possible
;;has the biggeset vy0.  When y is 0, vy = -vy0 (parabolic).

;;so we know the upper bound on vy:
;;-vy must be = ymin + g, = ymin + 1.

;;thus, our lowest depth - corresponding to highest point,
;;is whatever we can hit while staying within the x bounds,
;;where vy is equal to our upper bound.

;;since x is bounded pretty tightly, we have  small range to explore.

(defn vy-upper-bound [{:keys [ymin ymax]}]
  (- (inc ymin)))

(let [{:keys [xmin xmax] :as bounds} (->> (io/resource "day17input.txt")
                  slurp
                  txt->target)
      xlower (guess-right xmin xmax)
      yupper (vy-upper-bound bounds)]
  (-> (fire bounds xlower yupper)
      :ymax))

;;is there a lower bound on vy? Uknown.

;;solve 17.2

;;we can enumerate all possible points, given
;;given our set of vx, we can explore vy exhaustively or with some
;;heuristic.

;;our x bounds are actually...
;;xmin, xmax
(defn enumerate [{:keys [xmin xmax ymin ymax] :as bounds}]
  (let [xlower (guess-right xmin xmax)
        yupper (vy-upper-bound bounds)]
    (vec (for [vx (range xlower (inc xmax))
               vy (range ymin   (inc yupper))
               :when (-> bounds (fire vx vy) :state hit?)]
           [vx vy]))))

(->> (io/resource "day17input.txt")
     slurp
     txt->target
     enumerate
     count)


;;OBE
;;none of this ended up being useful.  I thought I would
;;have to leverage the response surface as a parabolic curve,
;;and be able to find a global max for the height.  It works
;;fine, except you miss out on the ability to hit the minimal
;;point in the boundaries by taking a non-improving step,
;;so we end up with a local max.  Curious is this can be tweaked,
;;but for now, meh!

;; ;;we can probably binary search this.  I think iterative deepening is
;; ;;fine.
;; (defn seek-right [{:keys [xmin xmax] :as bounds}]
;;   (let [vx0 (guess-right xmin xmax)]
;;     (loop [vx vx0]
;;       (let [res (fire bounds vx 0)]
;;         (case (res :state)
;;           :center-center [vx 0]
;;           (:left-center :left-below :left-up) (recur (inc vx))
;;           :right (recur (dec vx)))))))

;; (defn seek-up [bounds vx0 vy0 ymax0]
;;   (loop [vx   vx0
;;          vy   (inc vy0)
;;          ymax ymax0
;;          best vy0]
;;     (let [res (fire bounds vx vy)]
;;       (case (res :state)
;;         :center-center (if (> (res :ymax) ymax)
;;                          (recur vx (inc vy) (res :ymax) vy)
;;                          [vx best ymax])
;;         [vx best ymax]))))

;; (defn hit? [state]
;;   (= state :center-center))

;; (defn step [dir bounds vx0 vy0 ymax]
;;   (let [l (case dir :left (dec vx0) (inc vx0))
;;         lmax (fire bounds l vy0)]
;;     (when (hit? (lmax :state))
;;       (seek-up bounds l vy0 (lmax :ymax)))))

;; ;;in theory, we are at a maximum for this vx.
;; ;;our question is to search left or right.
;; (defn max-direction [bounds vx0 vy0 ymax0]
;;   (let [{:keys [ymax state] :as l} (fire bounds (dec vx0) vy0)
;;         [lx ly lymax] (step :left bounds vx0 vy0 ymax0)
;;         [rx ry rymax] (step :right bounds vx0 vy0 ymax0)]
;;     (cond (or (= ymax0 lymax) (= ymax0 rymax))
;;             [vx0 vy0 ymax0] ;;done, found a peak.
;;           (and lymax rymax) ;;both valid, pick the greater.
;;             (if (< ymax0 lymax)
;;               [:left  [lx ly lymax]]
;;               [:right [rx ry rymax]])
;;           lymax
;;             (if (< ymax0 lymax)
;;               [:left [lx ly lymax]]
;;               [vx0 vy0 ymax0])
;;           rymax
;;             (if (< ymax0 rymax)
;;               [:right [rx ry rymax]]
;;               [vx0 vy0 ymax0]))))

;; (defn seek-max
;;   ([bounds vx0 vy0 ymax0]
;;    (let [res (max-direction bounds vx0 vy0 ymax0)]
;;      (if-not (keyword? (first res))
;;        res
;;        (let [[dir [vx0 vy0 ymax]] res]
;;          (loop [acc [vx0 vy0 ymax]]
;;            (if-let [[x1 y1 ymax1 :as nxt] (step dir bounds vx0 vy0 ymax)]
;;              (if (or (= ymax1 ymax) (< ymax1 ymax))
;;                [vx0 vy0 ymax]
;;                (recur nxt))
;;              [vx0 vy0 ymax]))))))
;;   ([bounds]
;;    (let [[vx0 vy0] (seek-right bounds)
;;          [vx0 vy0 ymax] (seek-up bounds vx0 vy0 0)]
;;      (seek-max bounds vx0 vy0 ymax ))))
