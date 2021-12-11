(ns aoc2021.util)

(defn brackets [s]
  (str "[" s "]"))

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
