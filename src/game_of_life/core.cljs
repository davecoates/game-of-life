(ns game-of-life.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [clojure.string :as string]
            [game-of-life.patterns :refer [available-patterns]]
            [game-of-life.canvas :as canvas]
            [cljs.reader :as reader]
            [goog.events :as events]
            [goog.dom :as gdom])
  (:import [goog.net XhrIo]
           goog.net.EventType
           [goog.events EventType]))

(enable-console-print!)


(defn torus
  "Wrap x,y if out of bounds defined by w(idth), h(eight)"
  [w h [x y]]
  (let [translate-coord (fn [x s]
                          (cond (> x w) (- x w 1)
                                (< x 0) (+ x w 1)
                                :else x))]
  [(translate-coord x w)
   (translate-coord y h)]))

(defn in-range?
  [[max-x max-y ][x y]]
    (and (<= 0 x max-x) (<= 0 y max-y)))

(defn neighbours
  "Generate coords of neighbours for cell at x,y"
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (and (not= 0 dx dy))]
    [(+ x dx) (+ y dy)]))

(defn tick
  "Spawn next generation from living cells"
  [neighbours cells]
  (do
    (set (for [[cell n] (frequencies (mapcat neighbours cells))
               :when (or (= n 3) (and (= n 2) (cells cell)))
               ] cell))))

(defn tick! [neighbours] (swap! app-state assoc :cells (tick neighbours (@app-state :cells))))

(defn start [neighbours]
    (js/setInterval #(tick! neighbours) 200))

(def app-state (atom {:cells #{}
                      :width 800
                      :height 400
                      :total-cells 10000
                      }))


(defn canvas [data owner]
  (reify
    om/IDidMount
    (did-mount [this]
               (let [canvas (om/get-node owner) resolution (om/get-state owner :resolution)]
                 (canvas/draw-board! canvas resolution (:cells data))))
    om/IDidUpdate
    (did-update [this prev-props prev-state]
               (let [canvas (om/get-node owner) resolution (om/get-state owner :resolution)]
                 (canvas/draw-board! canvas resolution (:cells data))))
    om/IRenderState
    (render-state [this {:keys [width height]}]
            (dom/canvas #js {:width width :height height}))))

(defn calc-resolution
  "Calculate vertical and horizontal number of cells based on total number of
  cells and width / height of render area"
  [total-cells w h]
  (let[w-ratio (/ w h)
       h-ratio (/ h w)
       sqrt (.sqrt js/Math (* w-ratio total-cells))]
    [sqrt (* h-ratio sqrt)]))

(defn keywordstr->keyword
  "Take a keyword string (eg. :keyword) and conver it to a keyword"
  [keywordstr]
  (keyword (subs keywordstr 1)))

(defn center-cell
  "Center a cell of position x * h within area size-w * size-h"
  [[w h] [x y]]
  (let [w2 (window/Math.ceil (/ w 2))
        h2 (window/Math.ceil (/ h 2))
        x2 (window/Math.floor (/ x 2))
        y2 (window/Math.floor (/ y 2))
        ]
    [(- w2 x2) (- h2 y2)]))

(defn set-pattern!
  "Set current pattern. Requires resolution in order to center pattern."
  [resolution pattern]
  (swap! app-state assoc :cells (set (map #(into [] (map + (center-cell resolution (pattern :size)) %)) (pattern :cells)))))


(defn main-view [data owner]
  (reify
    om/IInitState
    (init-state [_]
                (let [{:keys [width height total-cells]} data
                      resolution (calc-resolution total-cells width height)
                      [pattern-key pattern] (first available-patterns)
                      ]
                  (set-pattern! resolution pattern)
                  {:events (chan)
                   :running? false
                   :interval nil
                   :width width
                   :height height
                   :resolution resolution
                   :selected-pattern pattern-key
       }))
    om/IWillMount
    (will-mount [_]
      (let [events (om/get-state owner :events) res (om/get-state owner :resolution)]
        (go (loop []
              (let [v (<! events)]
                (om/set-state! owner :running? (= v :start))
                (cond
                 (= v :start) (om/set-state! owner :interval (start (fn [cell] (filter #(in-range? res %) (neighbours cell)))))
                 (= v :stop) (js/clearInterval (om/get-state owner :interval)))
              (recur))))))
    om/IRenderState
     (render-state [this {:keys [events running? width height resolution selected-pattern]}]
             (dom/div nil
                      (apply dom/select #js {
                                             :onChange (fn [e] (let [label (keywordstr->keyword (.. e -target -value))
                                                                     pattern (available-patterns label)]
                                                                 (set-pattern! resolution pattern)))
                                             }
                                  (map #(dom/option #js {:value (first %)} ((second %) :title)) available-patterns))
                      (dom/button #js {:onClick (fn [e] (put! events
                                                              (if running? :stop :start)))
                                       } (if running? "Stop" "Start"))
                      (om/build canvas data {:state {:width width :height height :resolution resolution}})))))

(om/root main-view app-state
  {:target (. js/document (getElementById "app"))})

;(def canvas (js/document.getElementById "game"))

;(draw-board! canvas 1000 ten-engine-cordership)
;(draw-line (.getContext canvas "2d") [50 0] [50 1000])
