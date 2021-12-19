(ns aoc2021.util)


(defn enclose [s char]
  (case char
    \[ (str \[ s \])
    \{ (str \{ s \})
    \( (str \( s \))
    (str char s char)))

(defn brackets [s]
  (enclose s \[))

(defn braces [s]
  (enclose s \{))

(defn read-as-vector [txt]
  (-> txt
      brackets
      clojure.edn/read-string))

(defn memo-1 [f]
  (let [^java.util.HashMap cache (java.util.HashMap.)]
    (fn [x]
      (if-let [res (.get cache x)]
        res
        (let [res (f x)
              _ (.put cache x res)]
          res)))))

(def ^:dynamic *logging* nil)
(defn log [x]
  (when *logging*
    (println x))
  x)


(definterface I2D
  (getEntry ^long [^long r ^long c])
  (setEntry [^long r ^long c ^long v]))

;;redundant but meh.
(defn get-entry ^long [^I2D gr ^long x ^long y]
  (.getEntry gr x y))

(defn set-entry [^I2D gr ^long x ^long y ^long v]
  (.setEntry gr x y v))

;;no bounds checking lol.
(defrecord grid [^long w ^long h ^longs entries]
  I2D
  (getEntry ^long [this ^long x ^long y]
    (aget entries (+ (* y w) x)))
  (setEntry ^long [this ^long x ^long y ^long v]
    (do (aset entries (+ (* y w) x) v)
        this)))

(defn new-grid
  (^grid [^long w ^long h xs]
   (let [entries (if xs (long-array xs)
                     (long-array (* w h)))]
     (->grid w h entries)))
  (^grid [w h] (new-grid w h nil)))

(defn valid-entry ^long [^grid g ^long x ^long y ^long not-found]
  (if (and (> y -1) (< y (.-h g))
           (> x -1) (< x (.-w g)))
    (get-entry g x y)
    not-found))


(defn neighbors [x y]
  [[(inc x) y] ;;r
   [(dec x) y] ;;l
   [x (inc y)] ;;u
   [x (dec y)] ;;d
   [(inc x) (inc y)] ;;ur
   [(dec x) (inc y)] ;;ul
   [(dec x) (dec y)] ;;dl
   [(inc x) (dec y)] ;;dr
   ])

(defn neighbors4 [x y]
  [[(inc x) y] ;;r
   [(dec x) y] ;;l
   [x (inc y)] ;;u
   [x (dec y)] ;;d
   ])

(defn idx->xy [w h n]
  [(rem n w) (quot n h)])

(defn xy->idx [w h x y]
  (+ x (* y h)))

(defn adjacency [w h & {:keys [neighbors-fn] :or
                        {neighbors-fn neighbors}}]
  (->> (for [x (range w)
             y (range h)]
         (let [nebs (->> (neighbors-fn x y)
                         (filterv (fn [[x y]]
                                    (and (>= x 0) (< x w)
                                         (>= y 0) (< y h))))
                         (map (fn [[x y]]
                                (xy->idx w h x y))))]
           [(xy->idx w h x y) [x y] nebs]))
       (reduce (fn [acc [n xy nebs]]
                 (assoc acc n {:coord xy :neighbors nebs})) {})))

;;minimally inspired by spork.cljgraph
(defprotocol IFringe
  (peek-fringe [fr])
  (pop-fringe  [fr])
  (push-fringe [fr w nd]))

;;original lame priorityq impl.
(defrecord minpq [entries]
  IFringe
  (peek-fringe [fr]
    (when-let [v (first entries)]
      (v 1)))
  (pop-fringe  [fr]
    (if-let [k (first entries)]
      (minpq. (disj entries k))
      fr))
  (push-fringe [fr w nd]
    (minpq. (conj entries [w nd]))))

(defn min-pq [& xs]
  (minpq. (into (sorted-set-by
                 (fn [l r]
                   (let [res (compare (l 0) (r 0))]
                     (if-not (zero? res) res
                             (compare (l 1) (r 1)))))) xs)))

;;playing with indexed priority queue to see if we do better.
(defrecord minidxq [entries known]
  IFringe
  (peek-fringe [fr]
    (when-let [v (first (vals entries))]
      (first v)))
  (pop-fringe  [fr]
    (if-let [kv (first entries)]
      (let [k  (key kv)
            xs (val kv)
            v (first xs)]
        (minidxq. (if (> (count xs) 1)
                 (assoc  entries k (disj xs v))
                 (dissoc entries k))
               (dissoc known v)))
        fr))
  (push-fringe [fr k v]
    (if-let [oldk (known v)]
      (let [old     (entries oldk)]
        (minidxq. (-> (if (> (count old) 1)
                     (assoc entries oldk (disj old v))
                     (dissoc entries oldk))
                   (update k #(conj (or % #{}) v)))
               (assoc known v k)))
      (minidxq. (update entries k #(conj (or % #{}) v))
             (assoc known v k)))))

(defn min-indexed-pq [& xs]
  (let [groups (group-by first xs)
        known (reduce (fn [acc [w k]]
                        (assoc acc k w)) {} xs)]
    (minidxq. (into (sorted-map) (for [[k v] groups]
                                [k (set (map second v))]))
           known)))
