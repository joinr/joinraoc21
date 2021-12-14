(ns aoc2021.day14
  (:require [clojure.java.io :as io]
            [aoc2021.util :as u]))

(def sample
  "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(defn txt->state [txt]
  (let [xs (->> txt clojure.string/split-lines)
        [first _ & rules] xs]
    {:init  first
     :rules (->> (for [ln rules]
                   (mapv clojure.string/trim
                         (clojure.string/split ln  #"->" )))
                 (reduce (fn [acc [path [v]]]
                           (assoc-in acc path v)) {}))}))

;;go naive for now.

(defn naive-insert [rules xs]
  (let [final (atom nil)]
    (-> (interleave xs
                    (for [[l r] (partition 2 1 xs)]
                      (do (reset! final r)
                          ((rules l) r))))
        vec
        (conj @final))))

;;exponential growth in space.
(defn naive-step [{:keys [init rules] :as st}]
  {:init (naive-insert rules init)
   :rules rules})

(defn score [xs]
  (let [freqs (frequencies xs)
        [least most] (->> freqs
                          (sort-by val )
                          ((juxt first last)))]
    [least most]
    (- (val most) (val least))))

(defn nth-step [n state]
  (->> state
       (iterate naive-step)
       (take (inc n))
       last
       :init))

;;14.1

(->> (io/resource "day14input.txt")
     slurp
     txt->state
     (nth-step 10)
     score)

;;14.2 need better algorithm.
;;So if we look at this better, our intuition that a constant space
;;algorithm akin to the population model from lanternfish holds.
;;The difference here is that the population changes are encoded as
;;"insertion" rules.  If you look at the result of said rules, they
;;are presented as:
;;CH -> B, the impliciation if we enumerate the actual resulting
;;state is that, [C H] - 1 -> [C B] + 1, [B H] + 1
;;So, our state is now multiple pairs defined by the
;;insertion rules.

(defn txt->parts [txt]
  (let [xs (->> txt clojure.string/split-lines)
        [init _ & rules] xs
        rules (->> (for [ln rules]
                     (mapv clojure.string/trim
                           (clojure.string/split ln  #"->" )))
                   (reduce (fn [acc [[l r :as k] v]]
                             (assoc acc k [(str l v)  (str v r) (nth v 0)])) {}))
        parts (zipmap (set (concat (keys rules)
                                   (mapcat (fn [[l r v]] [l r])
                                           (vals rules))))
                      (repeat 0))
        letters (mapcat seq (keys parts))]
    {:init  init
     :rules rules
     :parts (->> (for [[l r] (partition 2 1 init)]
                   (str l r))
                 (reduce (fn [acc k]
                           (update acc k inc)) parts))
     :totals (merge (zipmap letters (repeat 0))
                    (frequencies init))}))

(defn update-parts [{:keys [rules parts totals] :as state}]
  (let [deltas
        (reduce-kv (fn delts  [deltas k n]
                     (if (pos? n)
                       (let [[l r v] (rules k)
                             _ (println [k l r v])]
                         (-> deltas
                             (assoc  k (- n))
                             (update l #(+ (or % 0) n))
                             (update r #(+ (or % 0) n))
                             (update-in [:totals v] #(+ (or % 0) n))))
                       deltas))
                      {}
                      parts)
        _ (println deltas)
        new-parts (reduce-kv (fn ps [acc k v]
                               (assoc acc k (+ (acc k) v)))
                             parts (dissoc deltas :totals)) 
        new-totals (reduce-kv (fn  [acc k v]
                                (assoc acc k (+ (acc k) v)))
                              totals (deltas :totals))]
    (assoc state :parts new-parts :totals new-totals)))

(defn active-parts [st]
  (if (string? st)
    (->> (partition 2 1 st)
         (map #(apply str %))
         (frequencies))
    (->> st
         :parts
         (reduce-kv (fn [acc k v]
                      (if (pos? v) (assoc acc k v) acc)) {}))))

(defn nth-parts [n state]
  (->> state
       (iterate update-parts)
       (take (inc n))
       last))
