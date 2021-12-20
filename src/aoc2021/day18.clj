(ns aoc2021.day18
  (:require [clojure.java.io :as io]
            [aoc2021.util :as u]))

;;snailfish, bah.

(def sample
  "[1,2]
[[1,2],3]
[9,[8,7]]
[[1,9],[8,5]]
[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]")

(defn txt->pairs [txt]
  (->> txt
       clojure.string/split-lines
       (map clojure.edn/read-string)))

(def sample-pairs (txt->pairs sample))

(defn explodel [[[x y] r]]
  {:left x
   :res [(+ y r) r]})

(defn exploder [[l [x y]]]
  {:right x
   :res  [(+ l x) y]})

(defn find-explode
  ([path depth lr]
  ;;looking for a leaf with depth = 4
  (cond (reduced? path) path
        (coll? lr)
        (let [ln (number? (lr 0))
              rn (number? (lr 1))]
          (cond (and ln rn) (when (>= depth 4) (reduced [path lr]))
            ln  (find-explode (conj path 1) (inc depth) (lr 1))
            rn  (find-explode (conj path 0) (inc depth) (lr 0))
            :else (or (find-explode (conj path 0) (inc depth) (lr 0))
                      (find-explode (conj path 1) (inc depth) (lr 1)))))
        :else nil))
  ([lr]
   (or (when-let [known (-> lr meta :explode)]
         [known (get-in lr known)])
       (when-let [res (find-explode '[] 0 lr)]
         @res))))

;;go up until we can go left, then go right until we can't anymore.
(defn left-sibling [tr path]
  (let [fork (loop [idx  (dec (count path))]
               (when (> idx -1)
                 (let [hd (path idx)]
                   (if (= hd 1)
                     (conj (subvec path 0 idx) 0)
                     (recur (dec idx))))))]
    (when fork
      (loop [path fork
             tr   (get-in tr fork)]
        (if (number? tr) path
            (let [r (tr 1)]
            (if (number? r)
              (conj path 1)
              (recur (conj path 1)
                     r))))))))

(def test-tree [[1 2] [[3 [4 5]] 6]])

;;go up until we can go right, then go left until we cant anymore.
(defn right-sibling [tr path]
  (let [fork (loop [idx  (dec (count path))]
               (when (> idx -1)
                 (let [hd (path idx)]
                   (if (= hd 0)
                     (conj (subvec path 0 idx) 1)
                     (recur (dec idx))))))]
    (when fork
      (loop [path fork
             tr   (get-in tr fork)]
        (if (number? tr) path
            (let [l (tr 0)]
              (if (number? l)
                (conj path 0)
                (recur (conj path 0)
                       l))))))))
(defn update-in? [m path f]
  (if path
    (update-in m path f)
    m))

(defn assoc-in? [m path v]
  (if path
    (assoc-in m path v)
    m))

(defn path-compare [l r]
  (let [bnd (min (count l) (count r))]
    (loop [idx 0]
      (if (== idx bnd)
        0
        (let [lv (l idx)
              rv (r idx)]
          (cond (< lv rv) -1
                (> lv rv) 1
                :else (recur (unchecked-inc idx))))))))

(defn mark-split [tr path v]
  (if (and path (>= v 10))
    (with-meta tr
      (update (meta tr) :splits #(assoc (or % (sorted-map-by path-compare)) path v)))
    tr))

(defn explode-pass [tr]
  (when-let [[path [l r]] (find-explode tr)]
    (let [tr (with-meta tr {})
          parent (pop path)
          lpath  (left-sibling  tr path)
          lv     (when lpath (+ (get-in tr lpath) l))
          rpath  (right-sibling tr path)
          rv     (when rpath (+ (get-in tr rpath) r))
          _  (u/log {:exploding path :from [l r]
                       :lpath lpath :lv lv
                       :rpath rpath :rv rv})]
      (-> tr
          (assoc-in  path 0)
          (assoc-in? lpath lv)
          (assoc-in? rpath rv)
          (mark-split lpath lv)
          (mark-split rpath rv)))))

(defn split-num [n]
  (let [l (quot n 2)
        k (rem  n 2)
        r (if (zero? k) l (inc l))]
    [l r]))

;;instead of relying on invalid cache info, we need to
;;traverse and find splits manually.  This guarantees finding
;;the first split.  -AAARGH -  If a split produces another possible
;;split, we have to process it immediately to preserve order of operations!
(defn naive-split
  ([path depth lr did-split]
   (if (number? lr)
     (if (> lr 9 )
       ;;split.
       (let [sp (split-num lr)
             _  (reset! did-split true)
             _ (u/log [:splitting path lr sp])]
         (cond (>= depth 4) ;;going to cause explode.
               (with-meta sp {:explode path})
               (or (> (sp 0) 9) (> (sp 1) 9))
                 (naive-split path depth sp did-split)
               :else  sp))
       lr)
     (let [[l r] lr
           lres (naive-split (conj path 0) (inc depth) l did-split)
           ]
       (if-let [p (-> lres meta :explode)]
         (with-meta [lres r] {:explode p})
         (let [rres (naive-split (conj path 1) (inc depth) r did-split)
               ]
           (if-let [p (-> rres meta :explode)]
             (with-meta [lres rres] {:explode p})
             [lres rres]))))))
  ([tr]
   (let [did-split (atom nil)
         res       (naive-split [] 0 tr did-split)]
     (when @did-split
       res))))

;;looking at input, it seems we have no addable numbers with splits or explosions.
;;they are all reduced.  so we can keep track of splits, which can only be caused
;;by addition->explosion.

;;we have to explode a number if splitting makes it > 4 nested though.
;;so one way to do it is to have split-pass use explode-pass.
(defn split-pass
  [tr]
  (naive-split tr)
  #_([tr splits]
   (loop [acc       tr
          remaining splits]
     (if-let [p (first (keys remaining))]
       (if-let [v     (get-in acc p)] #_(remaining p) ;;possible path exploded...
          (let [v2        (split-num v)
                remaining (dissoc remaining p)
                nxt       (assoc-in acc p v2)
                _ (u/log [:split p v v2])]
            (if (= (count p) 4) ;;gonna cause an explosion!
              (do (u/log [:splode!])
                  (vary-meta nxt #(assoc % :splits remaining)))
              (recur nxt remaining)))
          ;;path exploded away.
          (recur acc (dissoc remaining p)))
       (vary-meta acc #(dissoc % :splits)))))
  #_([tr]
    (when-let [splits (-> tr meta :splits)]
      (split-pass tr splits))))

(defn add [l r]
  (->> [l r]
       (iterate (fn [acc]
                  (u/log acc)
                  (if-let [res (explode-pass acc)]
                    (do (u/log [:explode res])
                        res) ;;no explodes, look for splits.
                    (when-let [splitted (split-pass acc)]
                      (do (u/log [:split splitted]))
                      splitted))))
       (take-while identity)
       last))

(defn add-them [xs]
  (reduce add xs))

(defn magnitude [lr]
  (if (number? lr)
    lr
    (let [[l r] lr]
      (+ (* 3 (magnitude l))
         (* 2 (magnitude r))))))

;;day 18.1
(-> (io/resource "day18input.txt")
    slurp
    u/read-as-vector
    add-them
    magnitude)

;;day 18.2

(defn largest-sum [xs]
  (reduce max (for [i (range (count xs))
                    j (range (count xs))
                    :when (not= i j)]
                (max (magnitude (add (xs i) (xs j)))
                     (magnitude (add (xs j) (xs i)))))))

(->> (io/resaouce "day18input.txt")
     slurp
     u/read-as-vector
     largest-sum)
