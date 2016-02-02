(ns air-soccer.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.math :refer :all]
            [play-clj.g2d-physics :refer :all]
            [play-clj.ui :refer :all]))

(declare air-soccer-game main-screen text-screen)

(def ^:const goal-size 64)
(def ^:const border-strength 5)

(defn create-arrow []
  (let [t (texture "Arrow.png")]
    (assoc t :arrow? true
           :x 36 :y 64
           :scale-x 2 :scale-y 2)))

(defn create-ball [screen x y]
  (let [t (texture "Ball.png")
        size 24
        radius (/ size 2)
        tiles (texture! t :split size size)
        tiles (->> (aget tiles 0)
                   (map texture*))

        anim (animation 0.2 tiles :set-play-mode (play-mode :loop))
        
        body (add-body! screen (body-def :dynamic))
        shape (circle-shape :set-radius radius
                            :set-position (vector-2 radius radius))
        fixture (fixture-def :density 0.1 :friction 0 :restitution 0 :shape shape)]
    (body! body :create-fixture fixture)
    (body-position! body x y 0)
    (assoc (first tiles)
           :body body
           :animation anim
           :width size :height size
           :spinning? false
           :tiles tiles
           :ball? true
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

(defn create-rect [width height]
  (let [s (shape :filled :set-color (color :green) :rect 0 0 width height)]
    s))

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
    (println h)
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

(defn stop-ball! [screen {:keys [ball?] :as e}]
  (if ball?
    (do
      (update! screen :current-player :player-1)
      (doto e
        (body! :set-linear-velocity (vector-2 0 0))))
    e))

(defn update-arrow! [{:keys [active-player] :as screen} entities]
  (map (fn [{:keys [arrow?] :as e}]
         (if arrow?
           (let [{:keys [x y width]} (find-first :ball? entities)]
             (assoc e :x width :y y))
           e)) entities))

(defscreen text-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    [(assoc (label "0" (color :red))
            :fps? true
            :x 5 :y 0)])
  
  :on-render
  (fn [screen entities]
    (letfn [(render-fps [entities]
              (map (fn [e]
                     (if (:fps? e)
                       (doto e
                         (label! :set-text (str (game :fps))))
                       e)) entities))]
      (->> entities
           (render-fps)
           (render! screen)))))

(defn restart-game! []
  (on-gl (set-screen! air-soccer-game main-screen text-screen)))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (let [screen (update! screen :renderer (stage) :camera (orthographic) :world (box-2d 0 0))]
      (width! screen 640)
      [(create-ball screen 75 100)
       (create-arrow)
       (create-lower-bounds screen)
       (create-upper-bounds screen)
       (create-left-bounds screen)
       (create-right-bounds screen)]))

  :on-key-down
  (fn [{:keys [key] :as screen} entities]
    (->> entities
         (map (fn [{:keys [ball?] :as e}]
                (if ball?
                  (cond 
                    (= key (key-code :space))
                    (do
                      (println (body! e :get-linear-velocity))
                      e)
                    (= key (key-code :enter))
                    (doto e
                      (body! :apply-linear-impulse (vector-2 500000 -1200000)
                             (body! e :get-world-center) true))
                    (= key (key-code :s))
                    (stop-ball! screen e)
                    (= key (key-code :r))
                    (restart-game!)
                    :else e) 
                  e)))))
  
  :on-render
  (fn [screen entities]
    (clear!)
    (->> entities
         (step! screen)
         (update-arrow! screen)
         (render! screen))))

(defscreen error-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    (label "ERROR!" (color :red)))
  
  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen entities)))

(defgame air-soccer-game
  :on-create
  (fn [this]
    (set-screen! this main-screen text-screen)))

(comment
  (set-screen-wrapper! (fn [screen screen-fn]
                         (try (screen-fn)
                              (catch Exception e
                                (.printStackTrace e)
                                (set-screen! air-soccer-game error-screen)))))
  
  (do
    (require '[air-soccer.core.desktop-launcher :as launcher])
    (launcher/-main))
  

  ;; RESET TO MAIN SCREEN  
  (on-gl (set-screen! air-soccer-game main-screen))

  (require '[play-clj.repl :as repl])
  (repl/e main-screen)

  )
