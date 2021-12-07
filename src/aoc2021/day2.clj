(ns aoc2021.day2
  (:require [clojure.java.io :as io]))

(def sample
  (->> [:forward 5
        :down 5
        :forward 8
        :up 3
        :down 8
        :forward 2]
       (partition 2)))

(defn move [acc [dir amt]]
  (case dir
    :forward  (update acc :position + amt)
    :down     (update acc :depth + amt)
    :up       (update acc :depth - amt)))

(defn exec [moves]
  (reduce move {:position 0 :depth 0} moves))

(def moves (->> (as-> (io/resource "day2input.txt") it
               (slurp it)
               (str "[" it "]"))
             (clojure.edn/read-string)
             (partition 2)
             (map (fn [[l r]] [(keyword (name l)) r]))))


;;solution 2.1

(->> (exec moves)
     ((juxt :position :depth))
     (apply *))

;;2.2
(defn move2 [{:keys [position depth aim] :as acc} [dir amt]]
  (case dir
    :forward  (-> acc
                  (assoc :position (+ position amt)
                         :depth    (+ depth (* aim  amt))))
    :down     (update acc :aim   + amt)
    :up       (update acc :aim   - amt)))

(defn exec2 [moves]
  (reduce move2 {:position 0 :depth 0 :aim 0} moves))

;;solution 2.2
(->> (exec2 moves)
     ((juxt :position :depth))
     (apply *))
