(ns game-of-life.core
  (:require [clojure.string :as string]))

(enable-console-print!)


(defn clear [canvas]
  (let [ctx (.getContext canvas "2d")]
    (.save ctx)
    (.setTransform ctx 1 0 0 1 0 0)
    (.clearRect ctx 0 0 (.-width canvas) (.-height canvas))
    (.restore ctx))
  )


(defn rgb-str [r g b]
  (clojure.string/join ["rgb(", (clojure.string/join ", " [r g b]), ")"])
  )

(defn draw-cell [ctx size coord]
  (let [[x y] coord]
    (.fillRect ctx (* x size) (* y size) size size)))


(defn translate-coord [x w]
  (cond (> x w) (- x w 1)
        (< x 0) (+ x w 1)
        :else x))


(defn translate-cell [w h cell]
  (let [[x y] cell]
    [(translate-coord x w)
     (translate-coord y h)]))

(defn draw-board! [canvas size live-cells]
  (let [ctx (.getContext canvas "2d")
        cell-w (/ (.-width canvas) size)]
    (clear canvas)
    (set! (.-fillStyle ctx) (rgb-str 200 0 0))
    (doseq [cell live-cells] (draw-cell ctx cell-w (translate-cell 199 199 cell)))
    [live-cells]))

; [1 1] => [[0 0] [0 1] [0 2] [1 0] [1 2] [2 0] [2 1] [2 2]]
(defn neighbours [[cell-x cell-y] cell]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ cell-x dx) (+ cell-y dy)]))


(defn valid-loc? [loc]
  (every? #(<= 0 %) loc))


(def glider #{[1 0][ 2 1] [0 2] [1 2] [2 2]})

(defn parse-count [c]
  (js/parseInt (if (empty? c) 1 c)))

(defn decode-line [line]
  (let [seqs (re-seq #"(([0-9]*)([a-z]))" line)
        runs (for [[_ _ n s] seqs] [(parse-count n) s])
        ]
    (loop [[n s] (first runs) tail (rest runs) i 0 result []]
      (cond
       (nil? n) result
       (= s \b) (recur (first tail) (rest tail) (+ i n) result)
       (= s \o) (recur (first tail) (rest tail) (+ i n) (into [] (concat result (range i (+ i n)))))
       )
    )))

(defn decode-RLE-str [c]
  (let [lines (clojure.string/split c "$")]
    (set (apply concat
           (for [[index line] (map vector (iterate inc 1) lines)]
             (map #(do [% index]) (decode-line line)))))))


(def glider-gun (decode-RLE-str "24bo11b$22bobo11b$12b2o6b2o12b2o$11bo3bo4b2o12b2o$2o8bo5bo3b2o14b$2o8bo3bob2o4bobo11b$10bo5bo7bo11b$11bo3bo20b$12b2o"))

(def two-gun (decode-RLE-str "27bo11b$25bobo11b$15b2o6b2o12b2o$14bo3bo4b2o12b2o$3b2o8bo5bo3b2o14b$3b2o8bo3bob2o4bobo11b$13bo5bo7bo11b$14bo3bo20b$15b2o22b$26bo12b$27b2o10b$26b2o11b4$21b2o16b$9bobo10b2o15b$9bo2bo8bo17b$2o10b2o11b2o12b$2o8bo3b2o8bobo12b$5b2o5b2o9bo6b2o7b$4bo4bo2bo10bo2bo2bo2bo6b$9bobo11bo6b3o6b$24bobo5b3o4b$25b2o6bobo3b$35bo3b$35b2o"))

(def bi-gun (decode-RLE-str "11bo38b$10b2o38b$9b2o39b$10b2o2b2o34b$38bo11b$38b2o8b2o$39b2o7b2o$10b2o2b2o18b2o2b2o10b$2o7b2o39b$2o8b2o38b$11bo38b$34b2o2b2o10b$39b2o9b$38b2o10b$38bo!"))
(def big-gun (decode-RLE-str "11bo38b$10b2o38b$9b2o39b$10b2o2b2o34b$38bo11b$38b2o8b2o$39b2o7b2o$10b2o2b2o18b2o2b2o10b$2o7b2o39b$2o8b2o38b$11bo38b$34b2o2b2o10b$39b2o9b$38b2o10b$38bo"))

(def acorn (decode-RLE-str "bo5b$3bo3b$2o2b3o"))

(def ten-engine-cordership (decode-RLE-str "42bo45b$42bo45b$44bo5bo37b$43bo6bobo35b$42bo3bo2bo38b$43bo2bobob2o36b$48bob2o36b$62b2o24b$62b2o24b7$70b2o16b$26b2o2bo39b2o16b$29bobo56b$28bo59b2$30b2o56b2$31b2o55b$30bo47b2o8b$28b2ob2o45b2o8b$31b2o55b$16bo12bo58b$16bo22b3o46b$18bo5bo13bo49b$17bo6bobo10bo4b2o44b$16bo3bo2bo12bo3bo47b$17bo2bobob2o10bo2bo4bo41b2o$22bob2o10bo3bo3bo32bo8b2o$37b2obob3o31bobo9b$40bo47b$41b4o31bo2bo8b$30b3o10b2o33b2o8b$29bo3bo45bo8b$28bo4bo54b$27bo3bo56b$27bo2bob3o53b$27bo7bo52b$2o2bo24bo3bobo40b2o10b$3bobo23bo3bob2o41bo9b$2bo28b3ob2o39b2o10b2$4b2o82b2$5b2o81b$4bo83b$2b2ob2o52bobo6bobo17b$5b2o51bo9bobo17b$3bo55bo2bo6bo18b$61b3o24b5$51bo36b$50bobo35b2$50bo2bo34b$7b2o43b2o34b$7b2o44bo34b5$50b2o36b$52bo35b$15b2o33b2o36b$15b2o71b5$33bobo6bobo43b$32bo9bobo43b$23b2o8bo2bo6bo44b$23b2o10b3o50b7$31b2o55b$31b2o"))
(defn translate-cells [cells v]
  (set (map #(map + % v) cells)))


(defn tick [cells]
  (set (for [[cell n] (frequencies (mapcat neighbours cells))
        :when (case n
                2 (cells cell)
                3 true
                false
                )
        ] cell)))

(def GRID_STATE (set (map #(into [] (map + [90 90] %)) bi-gun)))


(defn tick! [] (set! GRID_STATE (tick GRID_STATE)))

(defn start [] (let [canvas (js/document.getElementById "game")]
                             (js/setInterval #(draw-board! canvas 200 (tick!)) 100)
                             ))

(set! (.-onload js/window) start)

;(def canvas (js/document.getElementById "game"))

;(draw-board! canvas 1000 ten-engine-cordership)

