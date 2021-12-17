(ns aoc2021.day16
  (:require [clojure.java.io :as io]
            [aoc2021.util :as u]))


;;ugh, decoding and parsing.
;;very ugly parser to begin with.
;;we can probably far more simply parse
;;the hex into unsigned bits.

;;Performance is not a huge issue for now.
;;Following the rules is.

(def hex->bin
  {\0  [0 0 0 0]
   \1  [0 0 0 1]
   \2  [0 0 1 0]
   \3  [0 0 1 1]
   \4  [0 1 0 0]
   \5  [0 1 0 1]
   \6  [0 1 1 0]
   \7  [0 1 1 1]
   \8  [1 0 0 0]
   \9  [1 0 0 1]
   \A  [1 0 1 0]
   \B  [1 0 1 1]
   \C  [1 1 0 0]
   \D  [1 1 0 1]
   \E  [1 1 1 0]
   \F  [1 1 1 1]})

;;can stick in an int array if useful.  Maybe later for perf.
(def string1 "D2FE28")

(defn bits [hex] (into [] (mapcat hex->bin hex)))

;;lame...
(defn bits->int [xs]
  (let [n (atom (count xs))]
    (->> xs (reduce (fn [acc v]
                      (swap! n dec)
                      (if (zero? v)
                        acc
                        (+ acc (Math/pow 2 @n))))
                    0)
         long)))

(defn partition-vec [n xs]
  (let [bound (count xs)]
    (for [i (range 0 bound n)
          :let [u (+ i n)]
          :when (<= u bound)]
      (subvec xs i u))))

(defn read-bits [n contents]
  (assert (<= n (count contents)) "not enough bits")
  [(subvec contents 0 n) (subvec contents n)])

(defn read-template [kvs contents]
  (let [[parsed leftover]
        (reduce (fn [[acc remaining] [k n]]
                  (if (= n :*)
                    (reduced [(assoc acc k remaining) nil])
                    (let [[parsed nxt] (read-bits n remaining)]
                      [(assoc acc k parsed) nxt])))
                [{} contents] kvs)]
    (if leftover
      (assoc parsed :parse/remaining leftover)
      parsed)))

;;when we parse, we have a [res remaining]
(defn parse-header [bits]
  (let [{:keys [version packet-type contents]}
        (read-template {:version 3 :packet-type 3 :contents :*} bits)]
    {:version (bits->int version)
     :packet-type (bits->int packet-type)
     :contents contents}))

(defn packet-type [x]
  (case x
    4 :literal
    :operator))

;;we have an overarching parse function that will iteratively pull

(defmulti parse-packet (fn [nd] (packet-type (nd :packet-type))))

(defn parse-n-children
  [n contents]
  (loop [remaining contents
         idx 0
         acc []]
    (if (= idx n)
      {:remaining remaining
       :children acc}
      (let [hdr    (parse-header remaining)
            packet (parse-packet hdr)]
        (if-let [unparsed (packet :remaining)]
          (recur unparsed
                 (inc idx)
                 (conj acc (dissoc packet :remaining)))
          (throw (ex-info "not-enough!" {:n n :idx idx :children acc :hdr hdr :packet packet})))))))

(defn parse-all-children [contents]
  (loop [remaining contents
         acc []]
    (let [hdr    (parse-header remaining)
          packet (parse-packet hdr)
          unparsed (packet :remaining)]
      (if (> (count unparsed) 6) ;;header length.
        (recur unparsed (conj acc (dissoc packet :remaining)))
        {:remaining unparsed
         :children (conj acc (dissoc packet :remaining))}))))

;;literal
;;-- what was the length, in bits that we used from in
;;input.  The remainder is inferred from the length.
(defmethod parse-packet :literal
  [{:keys [version packet-type contents] :as p}]
  (let [chunks (->> contents
                    (partition-vec 5) ;;indexed subvec parts.
                    (reduce (fn [acc part]
                              (let [v (subvec part 1)]
                                (if (zero? (part 0))
                                  (reduced (conj acc v))
                                  (conj acc v)))) []))
        length (* (count chunks) 5)
        res    (->> chunks (apply concat) bits->int)]
    {:packet (-> p
                 (dissoc :contents)
                 (assoc  :value res :length length)
                 (with-meta {:contents contents}))
     :remaining  (second (read-bits length contents))}))

(defmethod parse-packet :operator
  [{:keys [version packet-type contents] :as p}]
  (let [[[length-type] packet-info] (read-bits 1 contents)
        p (dissoc p :contents)]
    (if (zero? length-type) ;;total length
      (let [{:keys [total-length remaining]}
            (read-template {:total-length 15 :remaining :*} packet-info)
            total-length  (bits->int total-length)
            [my-packets others] (read-bits total-length remaining)
            {:keys [children]}   (parse-all-children my-packets)]
        {:packet (assoc p :length total-length
                          :children (mapv :packet children))
         :remaining others})
      (let [{:keys [packet-count remaining]}
            (read-template {:packet-count 11 :remaining :*} packet-info)
            packet-count  (bits->int packet-count)
            {:keys [children remaining]} (parse-n-children packet-count remaining)]
        {:packet (assoc p :packet-count packet-count
                          :children (mapv :packet children))
         :remaining remaining}))))

(defn parse-node [bs]
  (-> (if (string? bs) (bits bs) bs)
      parse-header
      parse-packet
      :packet))

(defn packet-walk [xs]
  (tree-seq #(contains? % :children) :children xs))

(defn version-sum [xs]
  (->> xs packet-walk (map :version) (reduce +)))

;;solve 16.1
(->> (io/resource "day16input.txt")
     slurp
     parse-node
     version-sum)

;;16.2

(def ops
  {0 {:op :sum
      :f (fn [& xs] (apply + xs))}
   1  {:op :product
       :f (fn [& xs] (apply * xs))}
   2 {:op  :min
      :f (fn [& xs] (apply min xs))}
   3 {:op  :max
      :f (fn [& xs] (apply max xs))}
   4 {:op :literal
      :f (fn [x] (x :value))}
   5 {:op :gt
      :f (fn [l r]
           (if (> l r) 1 0))}
   6  {:op :lt
       :f (fn [l r]
            (if (< l r) 1 0))}
   7  {:op :eq
       :f (fn [l r]
            (if (= l r) 1 0))}})

(defn interpret [nd]
  (let [t (nd :packet-type)
        f (-> t ops :f)]
    (if-let [children (nd :children)]
      (let [args (mapv interpret children)]
        (apply f args))
      (f nd))))

(defn eval-node [nd]
  (->> nd
       parse-node
       interpret))

;;looks good...
;; aoc2021.day16> (eval-node "C200B40A82")
;; 3
;; aoc2021.day16> (eval-node "04005AC33890")
;; 54
;; aoc2021.day16> (eval-node "880086C3E88112")
;; 7
;; aoc2021.day16> (eval-node "CE00C43D881120")
;; 9
;; aoc2021.day16> (eval-node "D8005AC2A8F0")
;; 1
;; aoc2021.day16> (eval-node "F600BC2D8F")
;; 0
;; aoc2021.day16> (eval-node "9C005AC2F8F0")
;; 0
;; aoc2021.day16> (eval-node "9C0141080250320F1802104A08")
;; 1

;;16.2
(->> (io/resource "day16input.txt")
     slurp
     eval-node)
