(ns play-cljs-test.level
  (:require [play-cljs-test.utils :as u]
            [play-cljs-test.enemies :as e]))

(def timer (atom 0))

(def level (atom []))

(defn reset-level
  []
  (reset! timer 0)
  (reset! level []))

(defn create-sinusoid [x y]
  {:Item :Enemy :Movement :Enemy/Sinusoid :x x :y y :radius 10 :status :alive :shoot-timer 0})

(defn create-upsidedown-right [x y]
  {:Item :Enemy
   :Movement :Enemy/Upsidedown
   :Shootingstyle :Vertical-Right
   :x x
   :y y
   :radius 10
   :status :alive
   :shoot-timer 0})

(defn create-upsidedown-left [x y]
  {:Item :Enemy
   :Movement :Enemy/Upsidedown
   :Shootingstyle :Vertical-Left
   :x x
   :y y
   :radius 10
   :status :alive
   :shoot-timer 0})

(defn create-south-east [x y]
  {:Item :Enemy
   :Movement :Enemy/South-East
   :x x
   :y y
   :radius 10
   :status :alive
   :shoot-timer 0})

(defn create-north-west [x y]
  {:Item :Enemy
   :Movement :Enemy/North-West
   :x x
   :y y
   :radius 10
   :status :alive
   :shoot-timer 0})

(defn generate-level
  []
  (reset!
   level
   (-> []
       (conj {:time 0 :enemy (create-sinusoid 80 10)})
       ((fn [l]
          (concat l (map (fn [t] {:time t :enemy (create-sinusoid 80 10)}) (range 1000 5000 1000)))))
       ((fn [l]
          (concat l (map (fn [t] {:time t :enemy (create-sinusoid 110 10)}) (range 1000 5000 1000)))))
       ((fn [l]
          (concat l (map (fn [t] {:time t :enemy (create-sinusoid 140 10)}) (range 1000 5000 1000)))))
       (conj {:time 500 :enemy (create-upsidedown-right 50 10)})
       ((fn [l]
          (concat l (map (fn [y] {:time 5500 :enemy (create-upsidedown-right 50 y)}) (range -250 0 30)))))
       ((fn [l]
          (concat l (map (fn [y] {:time 5500 :enemy (create-upsidedown-left 250 y)}) (range -250 0 30)))))
       ((fn [l]
          (concat l (map (fn [t] {:time t :enemy (create-sinusoid 50 10)}) (range 7000 10000 500)))))
       ((fn [l]
          (concat l (map (fn [t] {:time t :enemy (create-sinusoid 80 10)}) (range 7000 10000 500)))))
       ((fn [l]
          (concat l (map (fn [t] {:time t :enemy (create-sinusoid 110 10)}) (range 7000 10000 500)))))
       ((fn [l]
          (concat l (map (fn [t] {:time t :enemy (create-sinusoid 140 10)}) (range 7000 10000 500)))))
       ((fn [l]
          (concat l (map (fn [t] {:time t :enemy (create-sinusoid 170 10)}) (range 7000 10000 500)))))
       ((fn [l]
          (concat l (map (fn [t] {:time t :enemy (create-sinusoid 200 10)}) (range 7000 10000 500)))))
       ((fn [l]
          (concat l (map (fn [t] {:time 11000 :enemy (create-south-east (+ 300 (* -1 t)) t)}) (range -160 0 20)))))
       ((fn [l]
          (concat l (map (fn [t] {:time 11000 :enemy (create-north-west t t)}) (range -160 0 20)))))
       (->>
        (sort-by :time)))))

(defn check-timer
  [delta-time]
  (swap! timer (fn [t] (+ t delta-time)))
  (let [ls @level
        t @timer]
    (e/add-enemies
     (doall (map :enemy
                 (take-while
                  (fn [step]
             (if (:time-start step)
               (< (:time-start step) t)
               (< (:time step) t))) ls))))
    (swap! level (fn [level] (remove #(< (:time %) t) level)))))

(defn check-game-win
  []
  (let [level-count (count @level)
        enemy-count (count @e/enemies)]
    (if (and (= 0 level-count) (= 0 enemy-count))
      :game-win)))
