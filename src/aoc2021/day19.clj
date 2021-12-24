(ns aoc2021.day19
  (:require [clojure.java.io :as io]
            [aoc2021.util :as u]))


(def sample
"--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14")

(defn txt->scanners [txt]
  (let [[scanned xs]
        (->> txt
             clojure.string/split-lines
             (reduce (fn [[scanned current :as acc] ln]
                       (case (count ln)
                         (0 1)  acc
                         (case (subs ln 0 2)
                           "--" [(conj scanned current) [(-> ln
                                                             (clojure.string/replace "--- scanner " "")
                                                             (clojure.string/replace " ---" ""))]]
                           [scanned (conj current (u/read-as-vector ln))] )))
                     [[] []]))]
    (->> (conj scanned xs)
         rest
         (map (fn [xs]
                [(first xs) (vec (rest xs))]))
         (into {}))))


(defn square [x] (* x x))

(defn distance [[lx ly lz] [rx ry rz]]
  (Math/sqrt (+ (square (- rx lx)) (square  (- ry ly)) (square  (- rz lz)))))


(defn all-pairs [[scanner points]]
  (for [i (range (count points))
        j (range (count points))
        :when (not= i j)]
    (let [l (points i) r (points j)]
      [l r (distance l r)])))

(defn candidates [m l r]
  (let [pl (group-by (fn [[_ _ d]]
                       (long d))
                     (all-pairs (find m l)))
        pr (group-by (fn [[_ _ d]]
                       (long d))
                     (all-pairs (find m r)))
        similar (clojure.set/intersection (set (keys pl)) (set (keys pr)))
        ]
    (->> (for [k similar]
           (let [ls (pl k)
                 rs (pr k)]
             [k (for [i (range (count ls))
                      j (range i (count rs))
                      :let [left (ls i)
                            right (rs j)]
                      :when (u/float= (left 2) (right 2))]
                  {l left r right})]
             ))
         (filter (comp seq second)))))


(defn unique-segments [m l r]
  (->> (candidates m l r)
       (filter (fn [[k v]] (= (count v) 1)))))

;;determining orientations.
;;they lead us to believe it's natural to model as
;;facing direction, up.
;;where facing could be x, y, z, -x, -y, -z,
;;and up could be n, s, e, w (y, -y, )

;;if we are facing z, up is north, then
;;we have identical coords.

;;so, we need some transforms to guess the orientation
;;of the scanner (relative to a base coordinate system).
;;If we assume scanner 0 is "true", x y z in a right
;;hand rule system, we can leverage rotation matrices
;;and mmult to get our transforms for any of the 24 orientations.

;;Then we pass our candidate points through all 24 to determine
;;an orientation (the one that transforms points from the
;;foreign coordinate system into points identical to our
;;true coordinates).

(defn rotate-x [theta]
  (let [ct (Math/cos theta)
        st (Math/sin theta)]
  [1 0   0
   0 ct (- st)
   0 st  ct]))

(defn rotate-y [theta]
  (let [ct (Math/cos theta)
        st (Math/sin theta)]
    [ct    0  st
     0     1  0
     (- st) 0 ct]))

(defn rotate-z [theta]
  (let [ct (Math/cos theta)
        st (Math/sin theta)]
    [ct (- st) 0
     st    ct  0
     0     0   1]))

(def I3 [1 0 0
         0 1 0
         0 0 1])

(defn rads [deg]
  (* deg (/ Math/PI 180)))

(defn miniscule? [^double x]
  (< (Math/abs x) 10E-10))

(defn precision-zero [xs]
  (mapv (fn [x] (if (miniscule? x) 0 x)) xs))
;;rotations to get us to y = north.
(def  n-e-s-w
  (concat [[:north I3]]
          (for [[dir theta] [[:east  90] [:south 180] [:west 270]]]
            [dir (precision-zero (rotate-z (rads theta)))])))

(defn columns [m]
  #_(map (juxt #(nth % 0) #(nth % 1) #(nth % 2)) (partition 3 m ))
  (let [rows (partition 3 m)]
    (for [i (range 3)]
      (map #(nth % i) rows))))

(defn transpose [m]
  (reduce into [] (columns m)))

(defn mmult [l r]
  (for [[x y z :as row] (partition 3 l)
        [u v w :as col] (if (= (count r) 3)
                              [r]
                             (columns r))]
    (reduce + (map * row col))))

(def facings
  (->> {"z"  I3
        "-z" (rotate-y (rads 180))
        "x"  (rotate-y (rads 90))
        "-x" (rotate-y (rads 270))
        "y"  (rotate-x (rads 270))
        "-y" (rotate-x (rads 90))}
       (reduce-kv
        (fn [acc fac xs]
          (assoc acc fac (precision-zero xs))) {})))

;;the general algorithm for our matrices is to
;;invert the facing, then invert the rotation.
;;so multiply by RF (I think).
(def common-transforms
  (->> (for [[fac x] facings
             [dir y] n-e-s-w]
         [(str fac "-" (name dir)) (mmult x y)])
       (into {})))

(def common-inversions
  (reduce-kv (fn [acc k v]
               (assoc acc k (transpose v))) {} common-transforms))

(defn transform [k pt] (vec (mmult (common-transforms k) pt)))
(defn invert    [k pt] (vec (mmult (common-inversions k) pt)))

;;points->segs


;;[-661 -816 -575] [423 -701 434]  1126.417773297279
;;[-345 -311 381] [-661 -816 -575] 1485.3827789495879

;;want to get
;;[[-345 -311 381] [-661 -816 -575] [423 -701 434]]
;;{:points [[-345 -311 381] [-661 -816 -575] [423 -701 434]]
;; :distances [1126.417773297279 1485.3827789495879]}
;;sort by x, y, z

(defn uniques [m l r]
  (let [ms (->> (unique-segments m l r)
                (map (comp first second)))
        ls (map #(get % l) ms)
        lpoints (->> (for [[p1 p2 dist] ls]
                       [p1 p2])
                     (apply concat)
                     distinct)
        rs (map #(get % r) ms)
        rpoints (->> (for [[p1 p2 dist] rs]
                       [p1 p2])
                     (apply concat)
                     distinct)]
    (when (and (seq ls) (seq rs))
      {:lefts ls :rights rs})))

;;is there a project where m x l = r?

(defn v= [ls rs]
  (every? identity
          (map u/float= ls rs)))

(defn v+  [l r]  (mapv + l r))
(defn v-  [l r]  (mapv - l r))
(defn dot [l r]  (mapv * l r))

(defn translate [[dx dy dz :as d] xs]
  (mapv (fn [[x y z :as v]]
          (v- v d)
          #_[(- x dx) (- y dy) (- z dz)])
        xs))

(defn common-segs [{:keys [lefts rights]}]
  (let [[l1 l2] lefts
        [r1 r2] rights
        common (fn [xs]
                 (reduce (fn [acc x]
                           (if (acc x) (reduced x)
                               (conj acc x))) #{} xs))
        is-not  (fn [z [x y]]
                  (if (= x z) y x))
        cl     (common (concat (subvec l1 0 2) (subvec l2 0 2)))
        cr     (common (concat (subvec r1 0 2) (subvec r2 0 2)))]
    {:left  [(is-not cl l1) cl (is-not cl l2)]
     :right [(is-not cr r1) cr (is-not cr r2)]}))

(defn orient [ls rs]
  (let [cls (translate (nth ls 1) ls)
        crs (translate (nth rs 1) rs)
        tx  (mapv +    (nth ls 1) (nth rs 1))
        ;_ (println [tx cls crs])
        ]
    (->>  (for [k (keys common-transforms)]
            (let [[l c r :as res] (map #(transform k %) cls)]
              (when (and (v= l (crs 0)) (v= r (crs 2)))
                (println [:oriented k res])
                [k res])))
          (filter identity)
          first)))

(defn project [tx ty orientation xs ys]
  (let [offsetx (translate tx  xs)
        offsety (translate ty  ys)
        projected (mapv #(mapv long (transform orientation %)) offsetx)
        targets   offsety
        common     (clojure.set/intersection (set projected) (set targets))
        p1         (first common)
        p1x        (v+ (invert orientation p1) tx)
        p1y        (v+ p1 ty)
        position-y (v- p1x (transform orientation p1y))]
    (with-meta
    {;;coordinates in xs and ys's mutual fixed coordinate system that
     ;;intersect
     :common common
     ;;coordinates from ys projected onto xs coordinate system, that intersect.
     :common-clean (mapv #(v+ (mapv long (invert orientation %)) tx) common)
     ;;the location of ys origin (the scanner).
     ;;translate to ty
     :location position-y}
      {:lefts  projected
       :rights targets})))

;;compare a pair of scanners, treating l as the base coordinate system for both.
(defn compare-scans [m l r]
  (let [segsl (m l)
        segsr (m r)]
    (when-let [us (uniques segsl segsr)]
      (let [{:keys [left right] :as common} (common-segs (us))
            orientation (orient left right)
            cl (nth left 1)
            cr (nth right 1)]
        (project  cl cr orientation segsl segsr)))))
