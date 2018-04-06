(ns play-cljs-test.bullet
  (:require [play-cljs-test.utils :as u]))

(def bullets
  (atom []))

(defn reset-bullets []
  (reset! bullets []))

(defn create-bullet
  [{:keys [x y]}]
  (swap! bullets conj
         {:Item :Bullet
          :Movement :Bullet
          :x x
          :y y}))

(defn kill-bullets
  []
  (swap! bullets
         (fn [bs]
           (remove u/outside-screen bs))))

(defmethod u/draw :Bullet [{:keys [x y]}]
  [:rect {:x x :y y :width 5 :height 5}])

(defmethod u/intercept [:Bullet :Enemy]
  [bullet enemy]
  (let [px (:x bullet)
        py (:y bullet)
        [x0 y0 x1 y1 x2 y2 x3 y3] (u/bounding-box enemy)]
    (u/point-inside-rectangle x0 y0 x1 y1 x2 y2 x3 y3 px py)))

(defmethod u/move :Bullet
  [bullet]
  (update bullet :y (fn [y] (- y 10))))
