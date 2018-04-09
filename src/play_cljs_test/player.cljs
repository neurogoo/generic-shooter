(ns play-cljs-test.player
  (:require [play-cljs-test.utils :as u]
            [play-cljs-test.bullet :as b]))

(def STEP_SIZE 5)

(defonce player
  (atom {:Item :Player
         :Movement :Player
         :score 0
         :lives 3
         :x 140
         :y 280
         :shoot-timer 0}))

(defn reset-player []
  (reset! player {:Item :Player
                  :Movement :Player
                  :score 0
                  :lives 3
                  :x 140
                  :y 280
                  :shoot-timer 0}))

(defmethod u/draw :Player
  [player]
  (let [x (:x player)
        y (:y player)]
    [:triangle {:x1 x :y1 (- y 10) :x2 (- x 20) :y2 (+ y 10) :x3 (+ x 20) :y3 (+ y 10)}]))

(defmethod u/bounding-box :Player
  [{:keys [x y]}]
  (let [width 40
        height 20
        [x0 y0] (u/upper-left-corner-rectangle x y width height)
        [x1 y1] [(+ x0 width) y0]
        [x2 y2] [(+ x0 width) (+ y0 height)]
        [x3 y3] [x0 (+ y0 height)]]
    [x0 y0 x1 y1 x2 y2 x3 y3]))

(defmethod u/intercept [:Player :Enemy]
  [player enemy]
  (let [[px0 py0 px1 py1 px2 py2 px3 py3] (u/bounding-box player)
        [ex0 ey0 ex1 ey1 ex2 ey2 ex3 ey3] (u/bounding-box enemy)]
    (u/rectangles-overlap [px0 py0 px1 py1 px2 py2 px3 py3] [ex0 ey0 ex1 ey1 ex2 ey2 ex3 ey3])))

(defmethod u/intercept [:Player :Enemy/Bullet]
  [player enemy-bullet]
  (let [[px0 py0 px1 py1 px2 py2 px3 py3] (u/bounding-box player)
        [ex0 ey0 ex1 ey1 ex2 ey2 ex3 ey3] (u/bounding-box enemy-bullet)]
    (u/rectangles-overlap [px0 py0 px1 py1 px2 py2 px3 py3] [ex0 ey0 ex1 ey1 ex2 ey2 ex3 ey3])))

(defn check-player-health
  []
  (if (< (:lives @player) 0)
    :dead
    :lives))

(defn draw-score []
  [:fill
   {:color "black"}
   [:text {:value (str "Score: " (:score @player))
           :x 10
           :y 10
           :size 8
           :font "Georgia"}]])

(defn draw-lives []
  [:fill
   {:color "black"}
   [:text {:value (str "Lives: " (:lives @player))
           :x 270
           :y 10
           :size 8
           :font "Georgia"}]])

(defn move-left []
  (swap! player
         (fn [p]
           (u/constrain-movement
            (assoc p :x (- (:x p) STEP_SIZE))))))

(defn move-right []
  (swap! player
         (fn [p]
           (u/constrain-movement
            (assoc p :x (+ (:x p) STEP_SIZE))))))

(defn move-up []
  (swap! player
         (fn [p]
           (u/constrain-movement
            (assoc p :y (- (:y p) STEP_SIZE))))))

(defn move-down []
  (swap! player
         (fn [p]
           (u/constrain-movement
            (assoc p :y (+ (:y p) STEP_SIZE))))))

(defn increase-score [score]
  (swap! player
         (fn [p]
           (update p :score #(+ score %)))))

(defn check-timer
  [delta-time]
  (swap! player (fn [p] (update p :shoot-timer #(+ % delta-time)))))

(defn shoot-bullet
  []
  (when (> (:shoot-timer @player) 50)
    (swap! player (fn [p] (assoc p :shoot-timer 0)))
    (b/create-bullet @player)))
