(ns play-cljs-test.core
  (:require [play-cljs.core :as p]
            [play-cljs-test.bullet :as b]
            [play-cljs-test.player :as pl]
            [play-cljs-test.utils :as u]
            [play-cljs-test.enemies :as e]
            [play-cljs-test.background :as bk]
            [play-cljs-test.enemybullet :as eb]
            [play-cljs-test.level :as l]))
(enable-console-print!)

(defonce game (p/create-game u/SCREEN-WIDTH u/SCREEN-HEIGHT))

(defonce state (atom {:status :started}))

(defn draw-items
  [items color]
  (reduce
   (fn [s item]
     (conj s (u/draw item)))
   [:fill {:color color}]
   items))

(defn reset-game
  []
  (reset! state {:status :running})
  (b/reset-bullets)
  (bk/create-background)
  (pl/reset-player)
  (l/reset-level)
  (l/generate-level)
  (e/reset-enemies)
  (eb/reset-enemy-bullets))

(defn pause-game
  []
  (swap! state (fn [s] (assoc s :status :paused))))

(defn continue-game
  []
  (swap! state (fn [s] (assoc s :status :running))))

(defn draw-gameplay
  []
  [[:fill {:color "lightblue"}
    [:rect {:x 0 :y 0 :width 300 :height 300}]]
   [:fill {:color "white"}
    (u/draw @pl/player)]
   (draw-items @bk/pixels "red")
   (draw-items @b/bullets "black")
   (draw-items @e/enemies "white")
   (draw-items @eb/enemy-bullets "white")
   (pl/draw-score)
   (pl/draw-lives)])

(defn draw-pause-screen
  []
  [[:fill {:color "white"}
    [:rect {:x 50 :y 50 :width 150 :height 100}]]
   [:fill
    {:color "black"}
    [:text {:value "PAUSED"
            :x 90
            :y 100
            :size 16
            :font "Georgia"}]]])

(defn draw-game-over-screen
  []
  [[:fill {:color "white"}
    [:rect {:x 40 :y 50 :width 220 :height 100}]]
   [:fill
    {:color "black"}
    [:text {:value "Game over. Press R to restart"
            :x 50
            :y 100
            :size 16
            :font "Georgia"}]]])

(defn check-player-enemy-collision
  []
  (swap!
   pl/player
   (fn [player]
     (if (= :hit (reduce
                  (fn [player enemy]
                    (if (u/intercept player enemy)
                      (reduced :hit)
                      player))
                  player
                  @e/enemies))
       (update player :lives dec)
       player))))

(defn check-player-enemy-bullet-collision
  []
  (swap!
   pl/player
   (fn [player]
     (if (= :hit (reduce
                  (fn [player enemy]
                    (if (u/intercept player enemy)
                      (reduced :hit)
                      player))
                  player
                  @eb/enemy-bullets))
       (update player :lives dec)
       player))))

(defn check-if-bullet-hit-enemy
  [enemy bullets]
  (reduce (fn [used-bullet bullet]
            (if (u/intercept bullet enemy)
              (do
                (pl/increase-score 10)
                (reduced bullet)))) nil bullets))

(defn check-bullet-enemy-collision
  []
  (let [[new-enemies new-bullets] (loop [enemies @e/enemies
                                         bullets @b/bullets
                                         new-enemies []]
                                    (if (seq enemies)
                                      (let [enemy (first enemies)
                                            used-bullet (check-if-bullet-hit-enemy enemy bullets)]
                                        (recur (rest enemies)
                                               (if (seq used-bullet)
                                                 (remove #(= used-bullet %) bullets)
                                                 bullets)
                                               (if (seq used-bullet)
                                                 (conj new-enemies (assoc enemy :status :dead))
                                                 (conj new-enemies enemy))))
                                      [new-enemies bullets]))]
    (reset! e/enemies new-enemies)
    (reset! b/bullets new-bullets)))

(def main-screen
  (reify p/Screen
    ; runs when the screen is first shown
    (on-show [this]
      (reset-game))
    ; runs when the screen is hidden
    (on-hide [this])
    ; runs every time a frame must be drawn (about 60 times per sec)
    (on-render [this]
      (case (:status @state)
        :game-over
        (do
          (p/render game (into [] (concat (draw-gameplay) (draw-game-over-screen))))
          (let [pressed-keys (p/get-pressed-keys game)]
            (cond
              (contains? pressed-keys u/RESET)
              (reset-game))))
        :paused
        (do
          (p/render game (into [] (concat (draw-gameplay) (draw-pause-screen))))
          (let [pressed-keys (p/get-pressed-keys game)]
            (cond
              (contains? pressed-keys u/ENTER)
              (continue-game))))
        :running
        (do
          (p/render game (draw-gameplay))
          (do
            (check-player-enemy-collision)
            (check-bullet-enemy-collision)
            (check-player-enemy-bullet-collision))
          (if (= :dead (pl/check-player-health))
            (swap! state (fn [s] (assoc s :status :game-over))))
          (let [total-time (p/get-total-time game)]
            (u/move-items b/bullets total-time)
            (u/move-items e/enemies total-time)
            (u/move-items bk/pixels total-time)
            (u/move-items eb/enemy-bullets total-time))
          (e/kill-enemies)
          (b/kill-bullets)
          (bk/kill-pixels)
          (eb/kill-enemy-bullets)
          (let [delta-time (p/get-delta-time game)]
            (bk/check-timer delta-time)
            (e/check-timer delta-time)
            (l/check-timer delta-time))
          (let [pressed-keys (p/get-pressed-keys game)]
            (cond
              (contains? pressed-keys u/LEFT_ARROW)
              (pl/move-left)
              (contains? pressed-keys u/RIGHT_ARROW)
              (pl/move-right)
              (contains? pressed-keys u/UP_ARROW)
              (pl/move-up)
              (contains? pressed-keys u/DOWN_ARROW)
              (pl/move-down))
            (cond
              (contains? pressed-keys u/SPACE)
              (b/create-bullet @pl/player)
              (contains? pressed-keys u/RESET)
              (reset-game)
              (contains? pressed-keys u/ENTER)
              (pause-game))))))))

; start the game
(doto game
  (p/start)
  (p/set-screen main-screen))
