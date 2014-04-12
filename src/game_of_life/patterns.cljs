(ns game-of-life.patterns
  (:require [clojure.string :as string]))

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


(defn parse-rule
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

(defn parse-rle
  "Parse Run Length Encoded file. See http://www.conwaylife.com/wiki/RLE"
  [rle]
  (let [lines (string/split-lines rle)
        comments (filter #(= "#" (first %)) lines)
        meta-data (parse-rle-headers comments)
        matches (first (re-seq #"(x[ ]?=.*)[\n\r]+([\s\S]*)" rle))
        {:keys [rule x y]} (parse-rule (second matches))
        pattern (last matches)
        ]
    {:title (-> (filter #(= "name" (:type %)) meta-data) first :val)
     :author (-> (filter #(= "author" (:type %)) meta-data) first :val)
     :comments (map :val (filter #(= "comment" (:type %)) meta-data))
     :size [(window/parseInt x) (window/parseInt y)]
     :rule rule
     :meta meta-data
     :cells (decode-RLE-str pattern)}))


;; From http://www.conwaylife.com/wiki/RLE
(def gosper-glider-gun "#N Gosper glider gun
#C This was the first gun discovered.
#C As its name suggests, it was discovered by Bill Gosper.
x = 36, y = 9, rule = B3/S23
24bo$22bobo$12b2o6b2o12b2o$11bo3bo4b2o12b2o$2o8bo5bo3b2o$2o8bo3bob2o4b
obo$10bo5bo7bo$11bo3bo$12b2o!")

(def acorn "#N Acorn
#O Charles Corderman
#C A methuselah with lifespan 5206.
#C www.conwaylife.com/wiki/index.php?title=Acorn
x = 7, y = 3, rule = B3/S23
bo5b$3bo3b$2o2b3o!")

(def five-engine-cordership "#N 5-engine Cordership
#O David Bell
#C A diagonal c/12 period 96 Cordership found on June 5, 2005.
#C www.conwaylife.com/wiki/index.php?title=5-engine_Cordership
x = 104, y = 75, rule = b3/s23
67b2o35b$68bo2bo4bo27b$65bo2b3obo2bobo26b$65bo2bo4b4o27b$65bo2bo35b$
68bo6bo28b$67b2obo33b$75bo11b2o15b$73b2o12b2o15b3$71bo32b$70bobo31b$
70bobo31b$71bo32b$95b2o7b$95b2o7b2$58bo45b$57b3o19bo24b$56b2ob2o17bobo
23b$57b3o18bobo23b$58bo20bo24b$58bobo43b$58b4o42b$61bo41bo2$57b2ob2o
42b$40b5o7bo3bo5bo41b$38b2o5b2o4bobo3bo3bo42b$38bo7bo4bo6bo45b$38b2o7b
o3bo2bo49b$25bo14b2o6bo3b3o49b$24b3o16bo60b$43bo4bo55b$24b3o17b2obo56b
$25b2ob2o74b$27bo76b3$b3o100b$2bo67b2o32b$2bo2bo63bobo32b$2bo2bo65bo
32b$3bobo98b$18b2o18b2o64b$18b2o18b2o64b$24bo79b$23b3o78b$2bo19b2ob2o
77b$bobo18bobo79b$2bo19bo81b2$46b2o56b$4o42b2o56b$2o3bo13bo84b$bo4bo
11bobo83b$3bob2o11bo85b$4bo15b2o82b$21bo82b$18b3o83b$19b2o83b$16bo87b$
16b2o86b$16b2o86b$6b2o6b2o88b$6b2o4b2o90b$12b2obo88b$14b2o88b5$14b2o
88b$14b2o!")

(def bi-gun "#N Bi-gun
#O Bill Gosper
#C A true period 46 double-barreled glider gun.
#C www.conwaylife.com/wiki/index.php?title=Bi-gun
x = 50, y = 15, rule = b3/s23
11bo38b$10b2o38b$9b2o39b$10b2o2b2o34b$38bo11b$38b2o8b2o$39b2o7b2o$10b
2o2b2o18b2o2b2o10b$2o7b2o39b$2o8b2o38b$11bo38b$34b2o2b2o10b$39b2o9b$
38b2o10b$38bo!")


(def patterns [gosper-glider-gun, bi-gun, acorn, five-engine-cordership])

(defn title->keyword
  [title]
  (-> title string/lower-case (string/replace " " "-") keyword))

;; Create map of patterns using title as keyword
(def available-patterns
  (reduce (fn [m a]
            (assoc m (title->keyword (:title a)) a))
          {}
          (map parse-rle patterns)))
