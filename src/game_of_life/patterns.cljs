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

(def glider #{[1 0][ 2 1] [0 2] [1 2] [2 2]})


(def glider-gun (decode-RLE-str "24bo11b$22bobo11b$12b2o6b2o12b2o$11bo3bo4b2o12b2o$2o8bo5bo3b2o14b$2o8bo3bob2o4bobo11b$10bo5bo7bo11b$11bo3bo20b$12b2o"))
(def bi-gun (decode-RLE-str "11bo38b$10b2o38b$9b2o39b$10b2o2b2o34b$38bo11b$38b2o8b2o$39b2o7b2o$10b2o2b2o18b2o2b2o10b$2o7b2o39b$2o8b2o38b$11bo38b$34b2o2b2o10b$39b2o9b$38b2o10b$38bo!"))
(def big-gun (decode-RLE-str "11bo38b$10b2o38b$9b2o39b$10b2o2b2o34b$38bo11b$38b2o8b2o$39b2o7b2o$10b2o2b2o18b2o2b2o10b$2o7b2o39b$2o8b2o38b$11bo38b$34b2o2b2o10b$39b2o9b$38b2o10b$38bo"))

(def acorn (decode-RLE-str "bo5b$3bo3b$2o2b3o"))

(def ten-engine-cordership (decode-RLE-str "42bo45b$42bo45b$44bo5bo37b$43bo6bobo35b$42bo3bo2bo38b$43bo2bobob2o36b$48bob2o36b$62b2o24b$62b2o24b7$70b2o16b$26b2o2bo39b2o16b$29bobo56b$28bo59b2$30b2o56b2$31b2o55b$30bo47b2o8b$28b2ob2o45b2o8b$31b2o55b$16bo12bo58b$16bo22b3o46b$18bo5bo13bo49b$17bo6bobo10bo4b2o44b$16bo3bo2bo12bo3bo47b$17bo2bobob2o10bo2bo4bo41b2o$22bob2o10bo3bo3bo32bo8b2o$37b2obob3o31bobo9b$40bo47b$41b4o31bo2bo8b$30b3o10b2o33b2o8b$29bo3bo45bo8b$28bo4bo54b$27bo3bo56b$27bo2bob3o53b$27bo7bo52b$2o2bo24bo3bobo40b2o10b$3bobo23bo3bob2o41bo9b$2bo28b3ob2o39b2o10b2$4b2o82b2$5b2o81b$4bo83b$2b2ob2o52bobo6bobo17b$5b2o51bo9bobo17b$3bo55bo2bo6bo18b$61b3o24b5$51bo36b$50bobo35b2$50bo2bo34b$7b2o43b2o34b$7b2o44bo34b5$50b2o36b$52bo35b$15b2o33b2o36b$15b2o71b5$33bobo6bobo43b$32bo9bobo43b$23b2o8bo2bo6bo44b$23b2o10b3o50b7$31b2o55b$31b2o"))

(def metacatacryst (decode-RLE-str "15366bo$15366bo$15364b2o$15363bo$15363bo$15363bo$15363bo6$15393bo$15392b2o$15390bo2bo2$15390bobo$15391bo133$15568b2o$15569b2o$15569bo29$15554bo$15553bobo$15555bo$15556bo507$59722b2o$59721b2o$59722bo29$59737bo$59736bobo$59736bo$59735bo13907$bo3bo$2bobo$o2bo$o$o21$33bo$32bo$31bo$32bo$33bo$29b3o!"))
