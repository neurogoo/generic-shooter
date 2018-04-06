(ns play-cljs-test.utils)

(enable-console-print!)

(defonce LEFT_ARROW 37)
(defonce RIGHT_ARROW 39)
(defonce UP_ARROW 38)
(defonce DOWN_ARROW 40)
(defonce SPACE 32)
(defonce SCREEN-WIDTH 300)
(defonce SCREEN-HEIGHT 300)
(defonce RESET 82)
(defonce ENTER 13)

(defn sin [x]
  (.sin js/Math x))

(defn center-of-rectangle [x y width height]
  (let [x-middle (/ (+ x width) 2)
        y-middle (/ (+ y height) 2)]
    [x-middle y-middle]))

(defn upper-left-corner-rectangle
  [x-middle y-middle width heigth]
  (let [x (- x-middle (/ width 2))
        y (- y-middle (/ heigth 2))]
    [x y]))

;; x0,y0 ----- x1,y1
;;  |            |
;; x3,y3------ x2,y2
(defn point-inside-rectangle
  [x0 y0 x1 y1 x2 y2 x3 y3 px py]
  (if (and (>= px x0)
           (<= px x1)
           (>= py y0)
           (<= py y3))
    true))

(defn rectangles-overlap
  [[x00 y00 x10 y10 x20 y20 x30 y30]
   [x01 y01 x11 y11 x21 y21 x31 y31]]
  (if (and (< x00 x21)
           (> x20 x01)
           (< y00 y21)
           (> y20 y01))
    true))

(defmulti bounding-box :Item)

(defmethod bounding-box :Player [{:keys [x y]}])

(defmethod bounding-box :Bullet [{:keys [x y width height]}]
  (let [[x y] (upper-left-corner-rectangle x y width height)]
    [x y width height]))

(defmulti draw :Item)

(defmulti intercept (fn [a b]
                      [(:Item a) (:Item b)]))

(defmethod intercept :default []
  #_(println "Ou nou"))

(defmulti move :Movement)

(defn outside-screen
  [{:keys [x y]}]
  (if (or (> x SCREEN-WIDTH) (< x 0) (> y SCREEN-HEIGHT) (< y 0))
    true))

(defn outside-screen-side
  [x y]
  (cond
    (> x SCREEN-WIDTH) :right
    (< x 0) :left
    (> y SCREEN-HEIGHT) :down
    (< y 0) :up
    :else false))

(defn constrain-movement
  [player]
  (let [{:keys [x y]} player]
    (-> player
        ((fn [p]
          (cond
               (> x SCREEN-WIDTH) (assoc p :x SCREEN-WIDTH)
               (< x 0 ) (assoc p :x 0)
               :else p)))
        ((fn [p]
          (cond
            (> y SCREEN-HEIGHT) (assoc p :y SCREEN-HEIGHT)
            (< y 0) (assoc p :y 0)
            :else p))))))

(defn move-items
  [items total-time]
  (swap! items
         (fn [is]
           (map #(move % total-time) is))))
