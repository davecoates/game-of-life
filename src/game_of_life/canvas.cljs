(ns game-of-life.canvas
  (:require [clojure.string :as string]))

(defn clear [canvas]
  (let [ctx (.getContext canvas "2d")]
    (.save ctx)
    (.setTransform ctx 1 0 0 1 0 0)
    (.clearRect ctx 0 0 (.-width canvas) (.-height canvas))
    (.restore ctx)))

(defn rgb-str [r g b]
  (clojure.string/join ["rgb(", (clojure.string/join ", " [r g b]), ")"]))

(defn draw-cell [ctx w h [x y]]
    (.fillRect ctx (* x w) (* y h) w h))

(defn draw-line [ctx [ax ay] [bx by]]
  (do
    (.beginPath ctx)
    (.moveTo ctx ax ay)
    (.lineTo ctx bx by)
    (.stroke ctx)))

(defn draw-board!
  [canvas [size-x size-y] live-cells]
  (let [ctx (.getContext canvas "2d")
        w (.-width canvas)
        h (.-height canvas)
        cell-w (/ w size-x)
        cell-h (/ h size-y)
        ]
    (clear canvas)
    (set! (.-fillStyle ctx) (rgb-str 200 0 0))
    (doseq [cell live-cells] (draw-cell ctx cell-w cell-h cell))
    (set! (.-lineWidth ctx) 0.1)
    (doseq [x (range 1 size-x)]
      (draw-line ctx [(* x cell-w) 0] [(* x cell-w) h]))
    (doseq [x (range 1 size-y)]
      (draw-line ctx [0 (* x cell-h)] [w (* x cell-h)]))
    ))
