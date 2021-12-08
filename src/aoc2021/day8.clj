(ns aoc2021.day8
  (:require [clojure.java.io :as io]
            [aoc2021.util :as u]))


(def sample
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(defn txt->samples [txt]
  (for [ln (-> txt
               clojure.string/split-lines)]
    (let [xs (clojure.string/split ln #"\|")]
      (mapv #(clojure.string/split (clojure.string/trim %) #" ") xs))))

(def samples (txt->samples sample))

(def n->segs
  {0 "abcefg"
   1 "cf"
   2 "acdeg"
   3 "acdfg"
   4 "bcdf"
   5 "abdfg"
   6 "abdefg"
   7 "acf"
   8 "abcdefg"
   9 "abcdfg"})

(def n->counts
  (reduce-kv (fn [acc n s]
               (assoc acc n (count s))) {} n->segs))

(def segs->n (zipmap (vals segs) (keys segs)))

(def lengths->nums
  (let [m (group-by val n->counts)]
    (->> (for [[length xs] m]
           [length (into #{} (map first xs))])
         (into {}))))

(def n->sets (zipmap (keys n->segs)  (map #(into #{}  %) (vals n->segs))))

(defn as-set [n]
  (if (set? n) n
      (n->sets n)))

(defn num- [l r]
  (clojure.set/difference (as-set l) (as-set r)))

(defn num& [l r]
  (clojure.set/intersection (as-set l) (as-set r)))

(defn num|| [l r]
  (clojure.set/union (as-set l) (as-set r)))

;;looks like we can derive cefdba based purely on set differences.
;;then we should be able to substitute into the system.
(def sub-rules
  (->> (for [i (range 10) j (range 10) :when (not= j i)]
         (let [res (num- i j)]
           (when (= (count  res) 1) [i j res])))
       (filter identity)))

(def and-rules
  (->> (for [i (range 10) j (range 10) :when (not= j i)]
         (let [res (num& i j)]
           (when (= (count  res) 1) [i j res])))
       (filter identity)))

;;day 8.1

(def all-uniques (reduce-kv (fn [acc length nums]
                              (if (= (count nums) 1)
                                (conj acc (first nums))
                                acc)) [] lengths->nums))

(defn counts [nums xs]
  (let [counts  (select-keys n->counts nums)
        lengths (set (vals counts))]
    (->> xs
         (map count)
         (filter lengths)
         count)))

(->> (io/resource "day8input.txt")
     slurp
     txt->samples
     (mapcat second)
     (counts [1 4 7 8]))
;;8.2

(def sample1
  (->> "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
       txt->samples
       first))

(def flat-sample (concat (first sample1) (second sample1)))

(defn n->set [n]
  (into #{} n))

(defn candidates [xs]
  (let [m (->> xs
               (map n->set)
               distinct
               (group-by #(lengths->nums (count %)))
               )
        ks (keys m)]
    (zipmap (map (fn [x] (if (= (count x) 1)
                           (first x)
                           x)) ks)
            (map (fn [xs]
                   (if (= (count xs) 1)
                     (first xs)
                     (set xs)))
                 (vals m)))))

;;decode using sets - 1 way.

;;first, find the numbers.  then use their
;;sets to isolate members and assign
;;abcdefg to ABCDEFG, to define a decoder.

;;we know 1, 7, 4 ,8 by uniqueness.
;;we can find 3 knowing that
;;3 || 1 = 3, so search for candidates by length among 2,3,5
;;5 || 7 = 9, so search for candidates by length among [2 5], from candidates in [0 6 9]
;;2 is candidates [2 3 5] - [3 5]
;;0 || 1 = 0, search for candidates by length among [0 6],
;;6 = [0 6] - [0]

;;I get the sense there is a way to search this out, or use logic programming
;;or a CP solver to find what I derived by observation.

;;all numbers are now known.
(defn idempotent [f n xs]
  (some (fn [r]
          (when (= (f n r) r)  r)) xs))

(defn find-3 [one xs]
  (idempotent num|| one xs))

(defn find-5-and-9 [seven fives sixes]
  (->> (for [l fives
             r sixes]
         [l r])
       (some (fn [[five six]]
               (when (= (num|| five seven) six)
                 [five six])))))

(defn find-zero [one xs]
  (idempotent num|| one xs))

(defn decode [xs]
  (let [known (atom (candidates xs))
        assign! (fn [n v]
                  (let [old-key (some (fn [x] (when (and (set? x)
                                                         (x n))
                                                x)) (keys @known))
                        pair  (= (count old-key) 2)
                        old-vals (@known old-key)
                        new-key (if pair
                                  (first (disj old-key n))
                                  (disj old-key n))
                        new-vals (if pair
                                   (first (disj old-vals v))
                                   (disj old-vals v))]
                    (reset! known
                            (-> @known
                                (assoc n v)
                                (dissoc old-key)
                                (assoc new-key new-vals)))))
        _ (assign! 3 (find-3 (@known 1) (@known #{3 2 5})))
        [five nine] (find-5-and-9 (@known 7) (@known #{2 5}) (@known #{0 6 9}))
        _ (assign! 5 five) ;;2 is inferred.
        _ (assign! 9 nine)
        _ (assign! 0 (find-zero (@known 1) (@known #{0 6})))
        ]
    (zipmap (vals @known) (keys @known))))

(defn decode-message [ins outs]
  (let [setify (fn [xs] (map #(into #{} %) xs))
        ins (setify ins)
        outs (setify outs)
        decoder (decode ins)]
    (map decoder outs)))

(defn decode-messages [txt]
  (for [[in out] (txt->samples txt)]
    (->> (decode-message in out)
         (apply str) ;;reader doesn't like longs with leading 0s...
         Long/parseLong)))

;;solve 8.2
(->> (io/resource "day8input.txt")
     slurp
     decode-messages
     (reduce +))
