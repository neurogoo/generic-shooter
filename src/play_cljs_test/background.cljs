(ns play-cljs-test.background
  (:require [play-cljs-test.utils :as u]))

(def timer (atom 0))

(def pixels
  (atom []))

(defmethod u/draw :Background
  [{:keys [x y]}]
  [:point {:x x :y y}])

(defmethod u/move :Background
  [pixel]
  (update pixel :y (fn [y] (+ y 1))))

(defn create-pixel
  [y]
  {:Item :Background
   :Movement :Background
   :x (rand-int 300)
   :y y})

(defn create-background
  []
  (do
    (reset! timer 0)
    (reset!
     pixels
     (reduce
      (fn [pixels y]
        (let [number-of-pixels-in-line (rand-int 5)]
          (into
           []
           (concat pixels (take number-of-pixels-in-line (repeatedly #(create-pixel y)))))))
      []
      (range 0 300 20)))))

(defn check-timer
  [delta-time]
  (swap! timer (fn [t] (+ t delta-time)))
  (if (> @timer 1000)
    (do
      (reset! timer 0)
      (swap! pixels
             (fn [ps]
               (let [number-of-pixels-in-line (rand-int 5)]
                 (into
                  []
                  (concat ps (take number-of-pixels-in-line (repeatedly #(create-pixel 0)))))))))))

(defn kill-pixels
  []
  (swap! pixels
         (fn [ps]
           (into [] (remove u/outside-screen ps)))))
