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


(defn rgb->color [r g b]
  (let [conv (fn [x] (-> x (/ 256.0) float))]
    (color (conv r) (conv g) (conv b) 1.0)))

(def color-1 (rgb->color 155 188 15))
(def color-2 (rgb->color 139 172 15))
(def color-3 (rgb->color 48 98 48))
(def color-4 (rgb->color 15 56 15))

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
        h goal-size
        s (create-rect w h :color (color :red))
        b (create-rect-body! screen w h)]
    (body-position! b 0 (- (center-y) (/ h 2)) 0)
    (assoc s
           :body b
           :goal? :left)))

(defn create-right-goal [screen]
  (let [w 1
        h goal-size
        s (create-rect w h :color (color :red))
        b (create-rect-body! screen w h)]
    (body-position! b (- (game :width) w) (- (center-y) (/ h 2)) 0)
    (assoc s
           :body b
           :goal? :right)))

(def goalie-socket-height 12)

(defn create-left-goalie [screen]
  (let [x 20
        y (center-y)
        t (texture "Goalie.png")
        w (texture! t :get-region-width)
        h goalie-socket-height
        b (create-rect-body! screen w h)]
    (body-position! b x y 0)
    [(assoc t :left-goalie? true :body b
            :x x :y y)
     (assoc (create-rect w h)
            :x x :y y)]))

(defn create-right-goalie [screen]
  (let [t (texture "Goalie.png" :flip true false)
        w (texture! t :get-region-width)
        h goalie-socket-height
        x (- (game :width) w 20)
        y (center-y)
        b (create-rect-body! screen w h)]
    (body-position! b x y 0)
    [(assoc t :right-goalie? true :body b
            :x x :y (center-y))
     (assoc (create-rect w h)
            :x x :y y)]))


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
