(ns aoc2021.day10
  (:require [clojure.java.io :as io]
            [aoc2021.util :s u]))

(def sample
  "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"
  )


(def opens  {\( \)
             \[ \]
             \{ \}
             \< \>})

(def scores { \) 3
             \] 57
             \} 1197
             \> 25137})

(def closers (zipmap (vals opens) (keys opens)))

(defn matches
  ([n depth pending xs]
   (if-let [x (first xs)]
     (if-let [terminal  (opens x)]
       (recur (inc n) (inc depth) (conj pending {:open x :expected terminal :from n :depth depth}) (rest xs))
       (if-let [{:keys [expected] :as m} (first pending)]
         (if (= expected x)
           ;;good match.
           (lazy-seq
            (cons (assoc m :matched n) (matches (inc n) (dec depth) (pop pending) (rest xs))))
           ;;bad match!
           [(assoc m :error "invalid close" :position  n :actual x :pending pending :remaining xs)])
         [{:error "no prior open" :position n  :open nil :expected (keys opens) :from nil :depth depth :actual x :pending pending
           :remaining xs}]))
     (when (seq pending)
       [{:incomplete "Unclosed chunks" :position n :depth depth :actual pending :remaining xs}])))
  ([xs] (matches 0 0 '() xs)))

(defn first-error [xs]
  (->> xs
       matches
       (some #(when (% :error) %))))

;;solution
(->> (io/resource "day10input.txt")
     slurp
     clojure.string/split-lines
     (keep first-error)
     (map :actual)
     (map scores)
     (reduce +))


;;day 10.2

(def complete-scores
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn score-it [xs]
  (->> xs
       (reduce (fn [acc x]
                 (+ (* 5 acc) (complete-scores x))) 0)))

(defn incompletes [xs]
  (->> xs
       matches
       (keep #(when (% :incomplete) %))))

(defn middle [xs]
  (let [n (count xs)
        _ (assert (odd? n) "expecting odd bro!")]
    (nth xs  (quot (dec n) 2))))

(defn finish [incompletes]
  (->> (map :expected incompletes)
       (apply str)))

(->>  (io/resource "day10input.txt")
      slurp
      clojure.string/split-lines
      (mapcat incompletes)
      (map (fn [xs]
             (->> (xs :actual)
                  (map :expected)
                  score-it)))
      sort
      middle)

