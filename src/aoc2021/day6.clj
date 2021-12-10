(ns aoc2021.day6
  (:require [clojure.java.io :as io]
            [aoc2021.util :as u]))

(def sample [3,4,3,1,2])
(def +reset+ 6)
(def +spawn+ 8)

(defn tick-fish [n]
  (case n
    0 :reset
    (dec n)))

(defn tick-fishes [xs]
  (let [[o s] (reduce-kv (fn [[original spawned] idx x]
                          (let [res (tick-fish x)]
                            (if (= res :reset)
                              [(assoc original idx +reset+)
                               (conj spawned +spawn+)]
                              [(assoc original idx res)
                               spawned])))
                         [xs []] xs)]
    (if-not (seq s)
      o
      (into o s))))

;;this is exponential in space lol.
(defn sim [init n]
  (->> (iterate tick-fishes sample)
       (take (inc n))
       last))

;;a better representation is to store the frequencies of states.
;;since they all grow at the same time.

(defn fish-counts [init & {:keys [end state-count] :or {end 6 state-count 10}}]
  (let [states (vec (repeat state-count 0))]
    (reduce (fn [v idx]
              (update v idx inc)) states init)))

;; start 0, end 6, count 7

;; 0                   ;spawning
;; [0 1 2 3 4 5 6][7 8 9]
;; [0 1 1 2 0 0 0][0 0 0]


;; start 1 end 6 count 7

;; 1
;; [6 0 1 2 3 4 5][7 8 9]
;; [0 1 1 2 0 0 0][0 0 1]

(defn ->cycle [start end entries]
  {:start start :end end :entries entries :count (inc end)})

(defn shift-right [{:keys [start end entries] :as cyc}]
  (assoc  cyc :start   (if (< start end) (inc start) 0)))

;;each day, advance the offset on the
;;general population.
(defn idx->local [cyc idx]
  (let [start  (cyc :start)]
    (rem (+ idx start) (cyc :count))))

(defn cycle-idx [cyc idx]
  (if (<= idx (cyc :end))
    (idx->local cyc idx)
     idx))

(defn cycle-nth [cyc idx]
  ((cyc :entries) (cycle-idx cyc idx)))

(defn cycle-assoc [cyc idx v]
  (update cyc :entries #(assoc % (cycle-idx cyc idx) v)))

(defn cycle-update [cyc idx f & args]
  (update cyc :entries #(apply update % (cycle-idx cyc idx) f args)))

;;basic update:

;; ;;shift the cycle. (shift-right)
;; ;;shift the spawns...
;; ;;   cycle_6 += spawn_7
;; ;;   spawn_7 <- spawn_8
;; ;;   spawn_8 <- spawn_9
;; ;;   spawn_9 <- cycle_0

(defn cycle-fishes [cyc]
  (let [new-cyc (shift-right cyc)]
    (-> new-cyc
        (cycle-update 6 + (cycle-nth cyc 7))
        (cycle-assoc  7   (cycle-nth cyc 8))
        (cycle-assoc  8   (cycle-nth cyc 9))
        (cycle-assoc  9   (cycle-nth cyc 1)))))

(defn print-fishes [cyc & {:keys [all]}]
  (let [bound (count (cyc :entries))
        bound (if all bound (dec bound))]
    (apply concat  (for [i (range bound)]
                     (let [n (cycle-nth cyc i)]
                       (repeat n i))))))

(defn fish-count [init t]
  (->> (iterate cycle-fishes init)
       (take (inc t))
       last
       :entries
       butlast
       (reduce +)))

(defn input->cycle [xs]
  (->cycle 0 6 (fish-counts xs)))


;;solution 6.1
(-> (->> (io/resource "day6input.txt")
         slurp
         u/brackets
         clojure.edn/read-string
         input->cycle)
    (fish-count 80))

;;easy, reused algo from before.

;;solution 6.2
(-> (->> (io/resource "day6input.txt")
         slurp
         u/brackets
         clojure.edn/read-string
         input->cycle)
    (fish-count 256))
