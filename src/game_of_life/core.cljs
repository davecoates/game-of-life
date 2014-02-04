(ns game-of-life.core
  (:require [clojure.string :as string]
            [game-of-life.patterns]))

(enable-console-print!)

(defn clear [canvas]
  (let [ctx (.getContext canvas "2d")]
    (.save ctx)
    (.setTransform ctx 1 0 0 1 0 0)
    (.clearRect ctx 0 0 (.-width canvas) (.-height canvas))
    (.restore ctx)))

(defn rgb-str [r g b]
  (clojure.string/join ["rgb(", (clojure.string/join ", " [r g b]), ")"]))

(defn draw-cell [ctx size [x y]]
    (.fillRect ctx (* x size) (* y size) size size))

(defn draw-line [ctx [ax ay] [bx by]]
  (do
    (.beginPath ctx)
    (.moveTo ctx ax ay)
    (.lineTo ctx bx by)
    (.stroke ctx)
  ))

; DAVE - wrap-loc in effect means what you see isn't really what's happening. Do behave like a torus it needs to be a part of
; the torus function. This is just display everything off screen on screen but the coords are still off screen...

(defn draw-board!
  [canvas size live-cells]
  (let [ctx (.getContext canvas "2d")
        wrap-at (- size 1)
        w (.-width canvas)
        h (.-height canvas)
        cell-w (/ w size)]
    (clear canvas)
    (set! (.-fillStyle ctx) (rgb-str 200 0 0))
    (doseq [cell live-cells] (draw-cell ctx cell-w (wrap-loc wrap-at wrap-at cell)))
    (set! (.-lineWidth ctx) 0.1)
    (doseq [x (range 1 size)]
      (draw-line ctx [(* x cell-w) 0] [(* x cell-w) h])
      (draw-line ctx [0 (* x cell-w)] [w (* x cell-w)]))
    ))

(defn wrap-loc
  "Wrap x,y if out of bounds defined by w(idth), h(eight)"
  [w h [x y]]
  (let [translate-coord (fn [x s]
                          (cond (> x w) (- x w 1)
                                (< x 0) (+ x w 1)
                                :else x))]
  [(translate-coord x w)
   (translate-coord y h)]))


(defn neighbours
  "Generate coords of neighbours for cell at x,y"
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ x dx) (+ y dy)]))


(defn tick
  "Spawn next generation from living cells"
  [cells]
  (set (for [[cell n] (frequencies (mapcat neighbours cells))
        :when (or (= n 3) (and (= n 2) (cells cell)))
        ] cell)))

(def GRID_STATE (set (map #(into [] (map + [40 40] %)) game-of-life.patterns/metacatacryst)))


(defn tick! [] (set! GRID_STATE (tick GRID_STATE)))

(defn start []
  (let [canvas (js/document.getElementById "game")]
    (js/setInterval #(draw-board! canvas 100 (tick!)) 100)))

(set! (.-onload js/window) start)

;(def canvas (js/document.getElementById "game"))

;(draw-board! canvas 1000 ten-engine-cordership)
;(draw-line (.getContext canvas "2d") [50 0] [50 1000])
