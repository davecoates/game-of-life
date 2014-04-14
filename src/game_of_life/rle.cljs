(ns game-of-life.rle
  (:require [clojure.string :as string]
            [game-of-life.patterns :refer [all-patterns]]
            ))


(def rule-names {
                 [[0 1 2 3 4 5 6 7 8] [3]] "Life without death"
                 [[2 3] [3]] "Conway's Life"
                 [[2 3] [3 6]] "HighLife"
                 [[1 2 3 4 5] [3]] "Maze"
                 [[0] [2]] "Seeds"
                 })



(defn parse-count [c]
  (js/parseInt (if (empty? c) 1 c)))

(defn decode-line
  [line]
  (let [seqs (re-seq #"(([0-9]*)([a-z]))" line)
        runs (for [[_ _ n s] seqs] [(parse-count n) s])]
    (loop [[n s] (first runs) tail (rest runs) i 0 result []]
      (cond
       (nil? n) result
       (= s \b) (recur (first tail) (rest tail) (+ i n) result)
       (= s \o) (recur (first tail) (rest tail) (+ i n) (into [] (concat result (range i (+ i n)))))))))

(defn decode-RLE-str [c]
  (let [lines (clojure.string/split c "$")]
    (set (apply concat
           (for [[index line] (map vector (iterate inc 1) lines)]
             (map #(vector % index) (decode-line line)))))))


(defn parse-rule-header
  "Parse rule string of form x = 36, y = 9, rule = B3/S23"
  [rule]
  (let [m (map #(string/split % #"=") (string/split rule #","))]
    ;; If rule isn't supplied default to B3/S23
    (into {:rule "B3/S23"} (for [[k v] m] [(-> k string/trim keyword) (string/trim v)]))))

(defn parse-rle-headers
  "Parse RLE headers. Each line starts with # followed by a single letter which
  indicates the type of header. Everything following is the value."
  [headers]
  (for [header headers
        :let [c (second header)
              value (string/trim (subs header 2))]
        :when (and (= (first header) "#") c)
        ]
    (condp = c
      "C" {:type "comment" :val value}
      "N" {:type "name" :val value}
      "O" {:type "author" :val value}
      {:type "?" :val value}
      )
  ))


(defn str->int
  [s]
  (let [s (if (empty? s) 0 s)]
    (js/parseInt s)))

(defn parse-rule
  "Split a rule string into two vectors. The first is the number of ON cells
  that cause an ON cell to remain ON. The second is the number of ON cells
  that cause an OFF cell to turn ON. eg. 23/3 becomes [[2,3] [3]] which
  means any cell with 2 or 3 neighbours stays on and any off cell with exactly
  3 neighbours turns on"
  [rule]
  (let [split-n (fn [n]
                  ;; Convert "XY" into (X,Y)
                  (let [v (map str->int
                               (filter not-empty (string/split n #"")))]
                    (if (empty? v) [0] (into [] v))))
        ;; Split into survival, birth
        [s b] (-> rule string/upper-case (string/split #"/"))
        ;; Reverse order if necessary
        [s b] (if (= (first s) "B") [b s]  [s b])
        ;; Remove S/B indicators if present
        [s b] [(string/replace s "S" "")
               (string/replace b "B" "")]
        ;; Convert into list of survival and birth values
        [s b] [(split-n s) (split-n b)]]
 [s b]
  ))



(defn parse-rle
  "Parse Run Length Encoded file. See http://www.conwaylife.com/wiki/RLE"
  [rle]
  (let [lines (string/split-lines rle)
        comments (filter #(= "#" (first %)) lines)
        meta-data (parse-rle-headers comments)
        matches (first (re-seq #"(x[ ]?=.*)[\n\r]+([\s\S]*)" rle))
        {:keys [rule x y]} (parse-rule-header (second matches))
        pattern (last matches)
        rule (parse-rule rule)
        ]
    {:title (-> (filter #(= "name" (:type %)) meta-data) first :val)
     :author (-> (filter #(= "author" (:type %)) meta-data) first :val)
     :comments (map :val (filter #(= "comment" (:type %)) meta-data))
     :size [(js/parseInt x) (js/parseInt y)]
     :rule rule
     :rule-title (rule-names rule)
     :meta meta-data
     :cells (decode-RLE-str pattern)}))


(defn title->keyword
  [title]
  (-> title string/lower-case (string/replace " " "-") keyword))

;; Create map of patterns using title as keyword
(def available-patterns
  (reduce (fn [m a]
            (assoc m (title->keyword (:title a)) a))
          (sorted-map)
          (map parse-rle all-patterns)))
