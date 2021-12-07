(ns aoc2021.day4
  (:require [clojure.java.io :as io]))

(def sample-data
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")


(defn read-data [txt]
  (let [lines (->> txt
                   clojure.string/split-lines
                   (filter (complement #{""})))
        nums   (first lines)
        boards (partition 5 (rest lines))]
    {:draws (clojure.edn/read-string (str "[" nums "]"))
     :boards (for [part boards]
               (clojure.edn/read-string
                (str "[ " (clojure.string/join " " part) "]")))}))

;;boards are abstractly a mapping of numbers -> entries,
;;entries -> row/col.
;;when we "mark" a number, we remove it from the entries,
;;entries are then removed from their corresponding row/col.

(defn ->board [entries]
  (let [rows (into [] (map vec) (partition 5 entries))
        cols (into [] (for [i (range 5)]
                        (set (map #(nth % i) rows))))]
    {:open (->> (for [[i row] (map-indexed vector rows)
                         [j v]   (map-indexed vector row)]
                     [v [i j]])
                   (into {}))
     :rows (mapv set rows)
     :cols cols
     :closed {}}))

(defn bingo-system [board-entries]
  (let [boards (mapv ->board board-entries)
        entries->boards
          (->> (for [[i b] (map-indexed vector boards)
                     entry (keys (b :open))]
                 [entry i])
               (reduce (fn [acc [entry i]]
                         (update acc entry #(conj (or % #{}) i )))
                       {}))]
    {:entries->boards entries->boards
     :boards boards}))

(defn input->bingo-system [txt]
  (let [{:keys [draws boards]} (read-data txt)]
    (merge {:draws draws} (bingo-system boards))))


;;given a bingo system, as we draw entries, we need to update boards.
;;we only update boards in which in the entries are open.
;;when we close entries on a board, we remove their membership from
;;affected row/col sets.
;;if we find any row/col set that is closed, we have a bingo.

(defn mark-board [{:keys [open rows cols closed] :as b} n]
  (let [[r c] (open n)
        new-rows (update rows r disj n)
        new-cols (update cols c disj n)
        bingo-row (-> r new-rows empty?)
        bingo-col (-> c new-cols empty?)]
    (assoc b :rows new-rows :cols new-cols
           :open   (dissoc open n)
           :closed (assoc closed n [r c])
           :bingo (when (or bingo-row bingo-col)
                    {:row (when bingo-row r) :col (when bingo-col c)}))))

(defn mark-boards [{:keys [entries->boards boards] :as sys} n]
   (let [affected   (entries->boards n)
         new-boards (reduce (fn [acc bidx]
                              (update acc  bidx mark-board n))
                            boards affected)
         new-entries (reduce (fn [acc bidx]
                               (let [ents (acc n)
                                     newents (disj ents bidx)]
                                 (if (seq newents)
                                   (assoc  acc n newents)
                                   (dissoc acc n)))) entries->boards affected)
         bingos     (->> affected
                         (map (fn [bidx]
                                (when-let [bingo (get-in new-boards [bidx :bingo])]
                                  [bidx bingo])))
                         (into {}))]
     (assoc sys :boards new-boards :entries->boards new-entries :bingo bingos)))

(defn draw [sys]
  (if-let [n (first (sys :draws))]
    (-> sys
        (assoc :draws (rest (sys :draws)) :drawn (conj (sys :drawn []) n))
        (mark-boards n))
    (assoc sys :bingo {:fail {:bingo (sys :bingo)}})))

(defn score-board [{:keys [open]} n]
  (* n (reduce + (keys open))))

(defn first-bingo [{:keys [boards draws] :as sys}]
  (let [res  (->> (iterate draw sys)
                  (drop-while #(empty? (% :bingo)))
                  first)
        bidx   (-> res :bingo keys first)
        winner (-> res :boards (get bidx))]
    (assoc res :score (score-board winner (last (res :drawn))))))

;;solution 4.1

(->> (io/resource "day4input.txt")
     slurp
     input->bingo-system
     first-bingo
     :score)

;;solution 4.2
(defn post-draw [{:keys [bingo boards entries->boards drawn remaining] :as sys}]
  (if (empty? bingo)
    sys
    (let [removed (set (keys bingo))
          new-entries (reduce-kv (fn [acc n bidxs]
                                   (assoc acc n (clojure.set/difference bidxs removed)))
                                 {} entries->boards)
          n             (last drawn)
          new-completed (into (sys :completed)
                              (map (fn [bidx]
                                     {:draw n :board (boards bidx) :score
                                      (score-board (boards bidx) n)})) removed)]
      res  (assoc sys
                  :entries->boards new-entries
                  :completed new-completed
                  :bingo {}
                  :remaining (- remaining (count removed))))))

(defn last-bingo [{:keys [boards draws] :as sys}]
  (let [nboards (count boards)
        sys     (assoc sys :remaining nboards :completed [])]
    (->> (iterate (comp post-draw draw) sys)
         (drop-while #(pos? (% :remaining)))
         first)))

(->> (io/resource "day4input.txt")
     slurp
     input->bingo-system
     last-bingo
     :completed
     last
     :score)
