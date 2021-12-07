(ns aoc2021.day3
  (:require [clojure.java.io :as io]))

;;note - can binary read using 2r
(def sample ["00100"
             "11110"
             "10110"
             "10111"
             "10101"
             "01111"
             "00111"
             "11100"
             "10000"
             "11001"
             "00010"
             "01010"])

;;intentionally dumb brute force way.  thinking of sorting, bitwise ops,
;;can't currently beat the complexity of naive traverse + accumulate.

(defn most-common
  ([n xs]
   (let [bound (long (inc  (/ n 2)))]
     (as-> xs it
       (reduce (fn [acc x]
                 (let [n (acc x 0)]
                   (if (= n bound)
                     (reduced (assoc acc :most x))
                     (assoc acc x (inc n)))))
               {} it)
       (or (it :most)
           (-> (sort-by #(-  (val %)) it)
               first
               key)))))
  ([xs] (most-common (count xs) xs)))

(defn gamma-chars [xs]
  (let [n (count xs)]
    (for [i (range (count (first xs)))]
      (most-common n (map #(nth % i) xs)))))

(defn read-bin [cs]
  (->> cs
       (apply str "2r")
       (clojure.edn/read-string)))

(defn gamma [gcs]
  (->> gcs
       read-bin))

(defn epsilon [gcs]
  (->> gcs
       (map {\0 \1 \1 \0})
       read-bin))

(defn power-consumption [xs]
  (let [gc (gamma-chars xs)]
    (* (gamma gc) (epsilon gc))))

;;solution 3.1
(->> (io/resource "day3input.txt")
     slurp
     clojure.string/split-lines
     power-consumption)

;;3.2

;;since we care about retention for possibly equality,
;;we dispense with short circuiting.

(defn bit-sieve [idx xs & {:keys [prefer]
                           :or {prefer \1}}]
  (let [groups (group-by #(nth % idx) xs)
        g0 (groups \0)
        g1 (groups \1)
        f0     (count g0)
        f1     (count g1)]
    (case prefer
      \1 (cond (> f0 f1) g0
               (> f1 f0) g1
               :else (groups prefer))
      \0 (cond (< f0 f1) g0
               (< f1 f0) g1
               :else (groups prefer)))))

(defn rating
  ([pref idx xs]
   (let [res (bit-sieve idx xs :prefer pref)]
     (if (> (count res) 1)
       (recur pref (inc idx) res)
       (first res))))
  ([pref xs] (rating pref 0 xs)))

(defn oxy-rating [xs] (read-bin (rating \1 xs)))
(defn c02-rating [xs] (read-bin (rating \0 xs)))

(defn life-support-rating [xs] (* (oxy-rating xs) (c02-rating xs)))

;;solution 3.2
(->> (io/resource "day3input.txt")
     slurp
     clojure.string/split-lines
     life-support-rating)
