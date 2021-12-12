(ns aoc2021.day12
  (:require [clojure.java.io :as io]
            [aoc2021.util :as u]))
(def sample
  "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(defn txt->edges [txt]
  (->> (clojure.string/split-lines txt)
       (map (fn [e]
              (clojure.string/split e #"-")))))

(defn adjacency [edges]
  (reduce (fn [acc [source sink]]
            (-> acc
                (update-in [source :sinks]
                           (fn [coll] (conj (or coll #{}) sink)))
                (update-in  [sink :sinks]
                           (fn [coll] (conj (or coll #{}) source)))))
          {} edges))

(defn big? [s] (= s (clojure.string/upper-case s)))

(defn depth-walk [adj from to]
  (let [bigs (->> adj keys (filter big?) set)]
    (loop [path    [from]
           pending []
           visited #{from}
           acc     []]
      (if (not (seq path))
        (if-let [{:keys [path visited]} (first  pending)]
          (recur path (rest pending) visited acc)
          acc)
        (let [nd (peek path)]
          (if (= nd to)
            (let [newacc (conj acc path)]
              (if-let [{:keys [path visited]} (first  pending)]
                (recur path (rest pending) visited newacc)
                newacc))
            (let [children    (->> nd adj :sinks
                                   (filter (fn [nd]
                                             (or (bigs nd) (not (visited nd))))))
                  visited (conj visited nd)]
              (if (seq children)
                (let [branch?     (> (count children) 1)
                      new-path    (conj path (first children))
                      new-pending (when branch?
                                    ;;branches
                                    (for [c (rest children)]
                                      {:path    (conj path c)
                                       :visited visited}))]
                  (recur new-path (into pending new-pending) visited acc))
                (recur nil pending visited acc)))))))))

;;solve 12.1
(-> (io/resource "day12input.txt")
     slurp
     txt->edges
     adjacency
     (depth-walk "start" "end")
     count)

;;day 12.1
(defn depth-walk2 [adj from to]
  (let [bigs (->> adj keys (filter big?) set)
        smalls (into #{} (filter (complement bigs)) (keys adj))]
    (loop [path      [from]
           pending   []
           visited   #{from}
           open      (disj smalls from)
           acc       []]
      (if (not (seq path))
        (if-let [{:keys [path visited open]} (first  pending)]
          (recur path (rest pending) visited open acc)
          acc)
        (let [nd (peek path)]
          (if (= nd to)
            (let [newacc (conj acc path)]
              (if-let [{:keys [path visited open]} (first  pending)]
                (recur path (rest pending) visited open newacc)
                newacc))
            (let [open      (cond (bigs nd) open ;;big caves no impact.
                                  (and (visited nd) (open nd)) #{} ;;already visited 1x, close.
                                  :else open) ;;unvisited, keep open.
                  visited   (if (bigs nd) visited (conj visited nd))
                  children  (->> nd adj :sinks
                                 (filter (fn [nd]
                                           (or (bigs nd)
                                               (open  nd)
                                               (not (visited nd))))))]
              (if (seq children)
                (let [branch?     (> (count children) 1)
                      new-path    (conj path (first children))
                      new-pending (when branch?
                                    ;;branches
                                    (for [c (rest children)]
                                      {:path      (conj path c)
                                       :visited   visited
                                       :open      open}))]
                  (recur new-path (into pending new-pending) visited open acc))
                (recur nil pending visited open  acc)))))))))

;;solve 12.2
(-> (io/resource "day12input.txt")
    slurp
    txt->edges
    adjacency
    (depth-walk2 "start" "end")
    count)


;;it occurs to me there is probably a permutations based variant here, as well as optimizations
;;we could do to ignore invalid subpaths.  We could, for example, perform the standard
;;dfs, then for each path, map a function over it to allow for one of the smaller caves
;;to be inserted along the path iff constraints are met.


;;given ["start" "A" "b" "end"]
;;are there any small caves we can add to the path, such that, we only
;;add 2 small caves?  We could deconstruct sub-paths, and try to create
;;cycles from them.
;;for each subpath, we could see if we can add a tour.

;;hmm, maybe future research on tourist paths.
