(ns aoc2021.util)

(defn brackets [s]
  (str "[" s "]"))

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
