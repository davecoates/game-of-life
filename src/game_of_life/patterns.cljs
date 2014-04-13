(ns game-of-life.patterns
  (:require [clojure.string :as string]))

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

(def queen-bee-loop"#N Queen bee loop
#O Bill Gosper
#C Period 30 oscillator originally found in 1970, similar to the queen
#C  bee shuttle.
#C www.conwaylife.com/wiki/index.php?title=Queen_bee_shuttle
x = 24, y = 24, rule = b3/s23
12bo11b$12bobo9b$13bobo8b$13bo2bo7b$13bobo8b$12bobo9b$12bo11b$3bo20b$
2bobo19b$bo3bo18b$2b3o19b$2o3b2o17b$17b2o3b2o$19b3o2b$18bo3bob$19bobo
2b$20bo3b$11bo12b$9bobo12b$8bobo13b$7bo2bo13b$8bobo13b$9bobo12b$11bo!")

(def ten-engine-cordership "#N 10-engine Cordership
#O Dean Hickerson
#C A c/12 period 96 diagonal Cordership that uses 10 switch engines, which was the fewest possible known at the time of its discovery.
#C www.conwaylife.com/wiki/index.php?title=10-engine_Cordership
x = 88, y = 88, rule = 23/3
42bo45b$42bo45b$44bo5bo37b$43bo6bobo35b$42bo3bo2bo38b$43bo2bobob2o36b$
48bob2o36b$62b2o24b$62b2o24b7$70b2o16b$26b2o2bo39b2o16b$29bobo56b$28bo
59b2$30b2o56b2$31b2o55b$30bo47b2o8b$28b2ob2o45b2o8b$31b2o55b$16bo12bo
58b$16bo22b3o46b$18bo5bo13bo49b$17bo6bobo10bo4b2o44b$16bo3bo2bo12bo3bo
47b$17bo2bobob2o10bo2bo4bo41b2o$22bob2o10bo3bo3bo32bo8b2o$37b2obob3o
31bobo9b$40bo47b$41b4o31bo2bo8b$30b3o10b2o33b2o8b$29bo3bo45bo8b$28bo4b
o54b$27bo3bo56b$27bo2bob3o53b$27bo7bo52b$2o2bo24bo3bobo40b2o10b$3bobo
23bo3bob2o41bo9b$2bo28b3ob2o39b2o10b2$4b2o82b2$5b2o81b$4bo83b$2b2ob2o
52bobo6bobo17b$5b2o51bo9bobo17b$3bo55bo2bo6bo18b$61b3o24b5$51bo36b$50b
obo35b2$50bo2bo34b$7b2o43b2o34b$7b2o44bo34b5$50b2o36b$52bo35b$15b2o33b
2o36b$15b2o71b5$33bobo6bobo43b$32bo9bobo43b$23b2o8bo2bo6bo44b$23b2o10b
3o50b7$31b2o55b$31b2o!")

(def wing-spaceship "#N Wing (spaceship)
#O Hartmut holzwart
#C The first extensible c/4 spaceship. Diagonal and period 4. Found in 1993.
#C www.conwaylife.com/wiki/index.php?title=Wing_(spaceship)
x = 23, y = 23, rule = b3/s23
4b2o17b$4bobo16b$4bo18b$7bo15b$3o4bobo13b$o6bo2bo3b3o6b$bo7b2o2bo2b2o
5b$3b3o7bo5b2o2b$13b2o4b3ob$4bobo3b3o9bo$5b2o2bo2bo6bo2bo$9bo9bo3b$9b
2o8bobob$6b3o14b$5bo2bo14b$5bo17b$5b2o16b$6bo16b2$7b2ob3o10b$7b2o14b$
8bo3bo10b$9b2o!")

(def gliders-by-the-dozen "#N Gliders by the dozen
#C A methuselah with lifespan 184 that emits exactly 12 gliders over the course of its evolution.
#C www.conwaylife.com/wiki/index.php?title=Gliders_by_the_dozen
x = 5, y = 3, rule = 23/3
2o2bo$o3bo$o2b2o!")

(def bomber-predecessor "#N Bomber predecessor
#O Nathan Thompson
#C A predecessor of the bomber spaceship in the HighLife rule.
#C www.conwaylife.com/wiki/index.php?title=HighLife
x = 10, y = 6, rule = 23/36
b3o6b$o9b$o9b$o8bo$9bo$9bo!")

(def replicator "#N Replicator
#O Nathan Thompson
#C A replicator for the HighLife rule (23/36).
#C www.conwaylife.com/wiki/index.php?title=HighLife
x = 5, y = 5, rule = 23/36
2b3o$bo2bo$o3bo$o2bob$3o!")

(def c3ladders "#N c/3 ladders
#O Dean Hickerson
#C Some uncommon c/3 ladders in Life without death.
#C www.conwaylife.com/wiki/index.php?title=Life_without_death
x = 14, y = 213, rule = b3/s012345678
3bobo8b$b7o6b$b3ob3o6b$9o5b$b8o5b$bob3obo6b$6o8b$bobo10b$b3o10b$2ob3o
8b$bob3o8b$b7o6b$8o6b$b3ob3o6b$b6o7b$3bo10b4$3bobo8b$b7o6b$bob3ob2o5b$
10o4b$b8o5b$b3ob3o6b$8o6b$bobobo8b$b5o8b$3obo9b$b6o7b$b6o7b$2obobo8b$b
6o7b$b6o7b$2obobo8b$b6o7b$b6o7b$3obo9b$b5o8b$b5o8b$3obo9b$b6o7b$b6o7b$
2obobo8b$b6o7b$b6o7b$2obobo8b$b6o7b$b6o7b$3obo9b$b5o8b$b5o8b$3obo9b$b
6o7b$b6o7b$2obobo8b$b6o7b$b6o7b$2obobo8b$b6o7b$b6o7b$3obo9b$b5o8b$b5o
8b$3obo9b$b6o7b$b6o7b$2obobo8b$b6o7b$b6o7b$2obobo8b$b6o7b$b6o7b$3obo9b
$b5o8b$bobobo8b$8o6b$b3ob3o6b$b8o5b$10o4b$bob3ob2o5b$b7o6b$3bobo8b4$3b
o10b$b5o8b$bob3o8b$7o7b$b5o8b$b3o10b$4o10b$bo12b$bo12b$3o11b$b3o10b$bo
b2o9b$6o8b$bob3o8b$b6o7b$3obobo7b$b7o6b$b2ob3o7b$9o5b$b9o4b$bob2o2b2o
5b$3ob3obo5b$b7o6b$b2ob3o7b$5obo7b$b6o7b$b4obo7b$3ob3o7b$b7o6b$b2ob3o
7b$5obo7b$b5o8b$b4o9b$3ob3o7b$b6o7b$b2ob3o7b$5o9b$b5o8b$b4o9b$3ob2o8b$
b5o8b$b2ob2o8b$5o9b$b5o8b$b4o9b$3obo9b$b3o10b$b2obo9b$5o9b$b5o8b$b4o9b
$3obo9b$b3o10b$b2o11b$4o10b$b4o9b$b3o10b$3o11b$b3o10b$b2o11b$4o10b$b3o
10b$bo12b$3o11b$b2obo9b$6o8b$b5o8b$b4o9b$2b3o9b$2bo11b4$b2obobobo5b$b
10o3b$3ob3ob3o3b$b12ob$b13o$5ob3ob3ob$b5ob3ob2ob$b3ob3ob3o2b$12o2b$b2o
b3ob3o3b$b4ob3obo3b$10o4b$b4ob3obo3b$b2ob3ob3o3b$12o2b$b2ob3ob3o3b$bob
2o2b2o2b2ob$13ob$b12ob$b2ob3ob3o3b$11o3b$b2obobobobo3b$b9o4b$2ob3ob3o
4b$b8o5b$bobobobo6b$8o6b$b2obobobo5b$b10o3b$3ob3ob3o3b$b11o2b$b11o2b$
2obo3bo6b$b10o3b$b10o3b$2ob3ob3o4b$b9o4b$bobobobo6b$8o6b$b4obo7b$b5o8b
$3obo9b$b4o9b$b3o10b$2ob2o9b$b4o9b$b2obo9b$5o9b$b3ob2o7b$b6o7b$7o7b$bo
b3o8b$b5o8b$3bo!")

(def life-without-death-quadratic "#N Life without death quadratic growth
#O Dean Hickerson
#C A pattern that grows quadratically (and eventually predictably) to
#C fill one quarter of the plane.
#C www.conwaylife.com/wiki/index.php?title=Life_without_death
x = 47, y = 47, rule = b3/s012345678
3b2o26b2o14b$3b4o23b5o12b$3b4o24b4o12b$b7o21b7o11b$b4obo22b4obo12b$8o
20b8o11b$bob4o22bob4o12b$8o20b8o3bo7b$b4obo22b4obo2b2o8b$8o20b8o2bo8b$
bob4o22bob4o12b$8o20b7o2bobobobo3b$b4obo22b17ob$8o20b10ob3ob3ob$bob4o
22b18o$8o20b19o$b4obo21b8ob3ob3obob$8o22b14o3b$bob4o23b2obobobobobo5b$
8o39b$b4obo40b$8o39b$bob4o40b$8o39b$b4obo40b$8o39b$bob4o40b$8o39b$b4ob
o40b$8o39b$bob4o40b$8o39b$b4obo40b$8o39b$bob4o40b$8o39b$b4obo40b$8o39b
$bob4o40b$7o2bobobobobobobobobobobobobobobobobobo3b$b45ob$10ob3ob3ob3o
b3ob3ob3ob3ob3ob3ob$b46o$47o$8ob3ob3ob3ob3ob3ob3ob3ob3ob3o3b$2b42o3b$
2b2obobobobobobobobobobobobobobobobobobobo!")

(def maze-wickstretcher "#N Maze wickstretcher
#C A common wickstretcher in the maze (12345/3) rule.
#C www.conwaylife.com/wiki/index.php?title=Maze
x = 6, y = 8, rule = 12345/3
2o4b$b3o2b$bob2ob$3b3o$3b2ob$bobo2b$b3o2b$2o!")

(def moon "#N Moon
#C A simple period-1 speed c spaceship in the \"seeds\" rule.
#C http://www.conwaylife.com/wiki/Moon
x = 2, y = 4, rule = B2/S
bo$o$o$bo!")


(def patterns [gosper-glider-gun, bi-gun, acorn, five-engine-cordership queen-bee-loop,
               ten-engine-cordership, wing-spaceship, gliders-by-the-dozen bomber-predecessor
               replicator c3ladders life-without-death-quadratic maze-wickstretcher
               moon])

(defn title->keyword
  [title]
  (-> title string/lower-case (string/replace " " "-") keyword))

;; Create map of patterns using title as keyword
(def available-patterns
  (reduce (fn [m a]
            (assoc m (title->keyword (:title a)) a))
          (sorted-map)
          (map parse-rle patterns)))
