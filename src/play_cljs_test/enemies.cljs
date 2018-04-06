(ns play-cljs-test.enemies
  (:require [play-cljs-test.utils :as u]
            [play-cljs-test.enemybullet :as eb]))
(enable-console-print!)

(def timer (atom 0))

(def enemies
  (atom []))

(defn reset-enemies
  []
  (reset! enemies []))

(defn add-enemies
  [es]
  (swap! enemies (fn [enemies] (concat enemies es))))

(defn kill-enemies
  []
  (swap!
   enemies
   (fn [es]
     (->> es
          (remove #(= :dead (:status %)))
          (remove #(= :down (u/outside-screen-side (:x %) (:y %))))))))

(defmethod u/draw :Enemy [{:keys [x y radius]}]
  [:ellipse {:x x, :y y, :width (* 2 radius), :height (* 2 radius)}])

(defmethod u/bounding-box :Enemy [{:keys [x y radius]}]
  (let [width (* 2 radius)
        height (* 2 radius)
        [x0 y0] (u/upper-left-corner-rectangle x y width height)
        [x1 y1] [(+ x0 width) y0]
        [x2 y2] [(+ x0 width) (+ y0 height)]
        [x3 y3] [x0 (+ y0 height)]]
    [x0 y0 x1 y1 x2 y2 x3 y3]))

(defmethod u/move :Enemy/Random
  [enemy]
  (let [x-add (- (rand-int 5) 2)
        y-add (- (rand-int 5) 2)
        x (:x enemy)
        y (:y enemy)
        new-x (if (u/outside-screen-side (+ x x-add) y) x (+ x x-add))
        new-y (if (u/outside-screen-side x (+ y y-add)) y (+ y y-add))]
    (-> enemy
        (assoc :y new-y)
        (assoc :x new-x))))

(defmethod u/move :Enemy/Sinusoid
  [enemy total-time]
  (let [x-add (u/sin (/ total-time 1000))
        y-add 1
        x (:x enemy)
        y (:y enemy)
        new-x (if (u/outside-screen-side (+ x x-add) y) x (+ x x-add))
        new-y (+ y y-add)]
    (-> enemy
        (assoc :y new-y)
        (assoc :x new-x))))

(defmethod u/move :Enemy/Upsidedown
  [enemy total-time]
  (let [y-add 1
        x (:x enemy)
        y (:y enemy)
        new-y (+ y y-add)]
    (-> enemy
        (assoc :y new-y))))

(defmulti shoot :Shootingstyle)

(defmethod shoot :default [enemy]
  (eb/create-regular-enemy-bullet enemy))

(defmethod shoot :Vertical [enemy]
  (eb/create-horizontal-enemy-bullet enemy))

(defn check-timer
  [delta-time]
  (swap!
   enemies
   (fn [enemies]
     (map
      (fn [enemy]
        (let [shoot-timer (:shoot-timer enemy)
              shoot-timer (+ shoot-timer delta-time)]
          (if (> shoot-timer 1500)
            (do
              (shoot enemy)
              (assoc enemy :shoot-timer 0))
            (assoc enemy :shoot-timer shoot-timer))))
      enemies)))
  (swap! timer (fn [t] (+ t delta-time))))
