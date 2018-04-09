(ns play-cljs-test.enemybullet
  (:require [play-cljs-test.utils :as u]))

(def enemy-bullets
  (atom []))

(defn reset-enemy-bullets
  []
  (reset! enemy-bullets []))

(defn create-regular-enemy-bullet
  [{:keys [x y]}]
  (swap! enemy-bullets conj
         {:Item :Enemy/Bullet
          :Movement :Enemy/Bullet
          :x x
          :y y}))

(defn create-horizontal-right-enemy-bullet
  [{:keys [x y]}]
  (swap! enemy-bullets conj
         {:Item :Enemy/Bullet
          :Movement :Enemy/Horizontal-Right
          :x x
          :y y}))

(defn create-horizontal-left-enemy-bullet
  [{:keys [x y]}]
  (swap! enemy-bullets conj
         {:Item :Enemy/Bullet
          :Movement :Enemy/Horizontal-Left
          :x x
          :y y}))

(defn kill-enemy-bullets
  []
  (swap! enemy-bullets
         (fn [bs]
           (remove u/outside-screen bs))))

(defmethod u/draw :Enemy/Bullet [{:keys [x y]}]
  [:rect {:x x :y y :width 3 :height 3}])

(defmethod u/bounding-box :Enemy/Bullet [{:keys [x y]}]
  (let [width 3
        height 3
        [x0 y0] (u/upper-left-corner-rectangle x y width height)
        [x1 y1] [(+ x0 width) y0]
        [x2 y2] [(+ x0 width) (+ y0 height)]
        [x3 y3] [x0 (+ y0 height)]]
    [x0 y0 x1 y1 x2 y2 x3 y3]))

(defmethod u/move :Enemy/Bullet
  [bullet]
  (update bullet :y (fn [y] (+ y 5))))

(defmethod u/move :Enemy/Horizontal-Right
  [bullet]
  (update bullet :x (fn [x] (+ x 5))))

(defmethod u/move :Enemy/Horizontal-Left
  [bullet]
  (update bullet :x (fn [x] (- x 5))))
