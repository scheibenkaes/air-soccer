(ns air-soccer.entities
    (:require [play-clj.core :refer :all]
              [play-clj.g2d :refer :all]
              [play-clj.math :refer :all]
              [play-clj.g2d-physics :refer :all]
              [play-clj.ui :refer :all]))

(defn create-arrow []
  (let [t (texture "Arrow.png")
        width (texture! t :get-region-width)
        height (texture! t :get-region-height)]
    (assoc t :arrow? true
           :x 36 :y 64 :width width :height height)))

(def ^:const ball-damping 0.03)

(defn create-ball [screen x y]
  (let [t (texture "Ball.png")
        size 24
        radius (/ size 2)
        tiles (texture! t :split size size)
        tiles (->> (aget tiles 0)
                   (map texture*))

        anim (animation 0.2 tiles :set-play-mode (play-mode :loop))
        
        body (add-body! screen (body-def :dynamic))
        shape (circle-shape :set-radius radius :set-position (vector-2 radius radius))
        fixture (fixture-def :density 0.1 :friction 0 :restitution 0  :shape shape)]
    (body! body :create-fixture fixture)
    (body! body :set-linear-damping ball-damping)
    (body-position! body x y 0)
    (assoc (first tiles)
           :body body
           :animation anim
           :width size :height size
           :spinning? false
           :tiles tiles
           :ball? true
           :radius radius
           :possesion :player1)))

(defn create-rect-body!
  [screen width height]
  (let [body (add-body! screen (body-def :static))]
    (->> [0 0
          0 height
          width height
          width 0
          0 0]
         float-array
         (chain-shape :create-chain)
         (fixture-def :density 1 :friction 0 :restitution 0.5 :shape)
         (body! body :create-fixture))
    body))

(def color-1-int 0x9bbc0f)

(def color-1 (color color-1-int))
(def color-2 (color 0x8bac0f00))
(def color-3 (color 0x30623000))
(def color-4 (color 0x0f380f00))

(def ^:const goal-size 64)
(def ^:const border-strength 5)

(defn create-rect [width height & {color :color :or {color color-1}}]
  (let [s (shape :filled :set-color color :rect 0 0 width height)]
    s))

(defn center-y []
  (-> (game :height) (/ 2)))

(defn center-x []
  (-> (game :width) (/ 2)))

(defn create-left-goal [screen]
  (let [w 1
        h (/ goal-size 2)
        s (create-rect w h :color (color :red))
        b (create-rect-body! screen w h)]
    (body-position! b 0 (- (center-y) (/ h 2)) 0)
    (assoc s
           :body b
           :goal? :left)))

(defn create-right-goal [screen]
  (let [w 1
        h (/ goal-size 2)
        s (create-rect w h :color (color :red))
        b (create-rect-body! screen w h)]
    (body-position! b (- (game :width) w) (- (center-y) (/ h 2)) 0)
    (assoc s
           :body b
           :goal? :right)))

(def goalie-body-size [24 12])

(defn create-left-goalie []
  (let [t (texture "Goalie.png")]
    (assoc t :left-goalie? true
           :x 20 :y (center-y))))

(defn create-right-goalie []
  (let [t (texture "Goalie.png" :flip true false)
        x (- (game :width) 60)]
    (assoc t :right-goalie? true
           :x x :y (center-y))))


(defn create-lower-bounds [screen]
  (let [s (create-rect 640 border-strength)
        b (create-rect-body! screen 640 border-strength)]
    (body-position! b 0 0 0)
    (assoc s :body b)))

(defn create-upper-bounds [screen]
  (let [s (create-rect 640 border-strength)
        b (create-rect-body! screen 640 border-strength)]
    (body-position! b 0 (- 320 border-strength) 0)
    (assoc s :body b)))

(defn create-side-bound [screen x y]
  (let [h (- (/ 320 2) (/ goal-size 2))
        s (create-rect border-strength h)
        b (create-rect-body! screen border-strength h)]
    (body-position! b x y 0)
    (assoc s :body b)))

(defn create-left-bounds [screen]
  (let [lower (create-side-bound screen 0 0)
        upper (create-side-bound screen 0 (+ (/ 320 2) (/ goal-size 2)))]
    [lower upper]))

(defn create-right-bounds [screen]
  (let [lower (create-side-bound screen (- 640 border-strength) 0)
        upper (create-side-bound screen (- 640 border-strength) (+ (/ 320 2) (/ goal-size 2)))]
    [lower upper]))

(defn create-pitch []
  (let [t (texture "Pitch.png")]
    (assoc t :pitch? true)))
