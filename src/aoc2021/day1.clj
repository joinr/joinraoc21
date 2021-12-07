(ns aoc2021.day1
  (:require [clojure.java.io :as io]))

;;part 1

(def sample [199 #_(N/A - no previous measurement)
             200 #_(increased)
             208 #_(increased)
             210 #_(increased)
             200 #_(decreased)
             207 #_(increased)
             240 #_(increased)
             269 #_(increased)
             260 #_(decreased)
             263 #_(increased)])

(def xs (->> (io/resource "day1input.txt")
             slurp
             clojure.string/split-lines
             (map (fn [x] (clojure.edn/read-string x))))) ;;slow but meh



(defn increases [xs]
  (->> xs (partition 2 1) (filter (fn [[l r]] (< l r)))))

;;day 1a solution
(count (increases xs))

;;part2

(def sample2 [199  ;A      
             200  ;A B    
             208  ;A B C  
             210  ;  B C D
             200  ;E   C D
             207  ;E F   D
             240  ;E F G  
             269  ;  F G H
             260  ;    G H
             263  ;      H
              ])

(defn increasing-parts [n coll]
  (->> coll
       (partition n 1)
       (map (fn [xs] (reduce + xs)))
       increases))

(count (increasing-parts 3 xs))
