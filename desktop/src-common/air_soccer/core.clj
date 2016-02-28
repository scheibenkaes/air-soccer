(ns air-soccer.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.math :refer :all]
            [play-clj.g2d-physics :refer :all]
            [play-clj.ui :refer :all]
            [air-soccer.screens.text-screen :as text-screen]))

(declare air-soccer-game main-screen)

(def ^:const goal-size 64)
(def ^:const border-strength 5)

(def color-1-int 0x9bbc0f)

(def color-1 (color color-1-int))
(def color-2 (color 0x8bac0f00))
(def color-3 (color 0x30623000))
(def color-4 (color 0x0f380f00))

(defn create-arrow []
  (let [t (texture "Arrow.png")
        width (texture! t :get-region-width)
        height (texture! t :get-region-height)]
    (assoc t :arrow? true
           :x 36 :y 64 :width width :height height)))

(defn- center-y []
  (-> (game :height) (/ 2)))

(defn- center-x []
  (-> (game :width) (/ 2)))

(def goalie-body-size [24 12])

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

(defn create-rect [width height & {color :color :or {color color-1}}]
  (let [s (shape :filled :set-color color :rect 0 0 width height)]
    s))

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

(defn stop-ball! [screen {:keys [ball?] :as e}]
  (if ball?
    (do
      (update! screen :current-player :player-1)
      (assoc
       (doto e
         (body! :set-linear-velocity (vector-2 0 0)))
       :spinning? false))
    e))

(defn update-arrow! [{:keys [active-player] :as screen} entities]
  (map (fn [{:keys [arrow? width height] :as e}]
         (let [{x-ball :x y-ball :y radius :radius :as ball} (find-first :ball? entities)]
           (if (and arrow? ball)
             (let [pos-ball (vector-2! (body! ball :get-position) :cpy)
                   pos-ball (vector-2! pos-ball :add radius radius)
                   {mouse-x :x mouse-y :y} (input->screen screen (input! :get-x) (input! :get-y))
                   mouse-pos (vector-2 mouse-x mouse-y)
                   sub (vector-2! (vector-2! pos-ball :cpy) :sub mouse-pos)                 
                   angle (vector-2! sub :angle)]
               (assoc e
                      :x (+ (x pos-ball) (/ width 2))
                      :y (- (y pos-ball) (/ height 2))
                      :angle angle
                      :vector sub
                      :origin-x (- (/ width 2))
                      :origin-y (/ height 2)))
             e))) entities))

(defn restart-game! []
  (on-gl (set-screen! air-soccer-game main-screen text-screen/text-screen)))


(defn- animate-spinning-ball [screen {:keys [animation spinning?] :as e}]
  (if spinning?
    (let [t (animation->texture screen animation)]
      (merge e t))
    e))

(def ^:const stop-ball-at "Length of a vector" 1600)

(defn ball-too-slow?
  "When the ball is getting so slow we want to stop it."
  [v]
  (-> (vector-2! v :len2) (< stop-ball-at)))

(defn- stop-ball-when-too-slow [screen {:keys [] :as e}]
  (let [speed (body! e :get-linear-velocity)]
    (if (ball-too-slow? speed)
      (stop-ball! screen e)
      e)))

(defn animate-ball [screen entities]
  (map (fn [{:keys [ball? spinning?] :as e}]
         (if ball?
           (->> e (animate-spinning-ball screen) (stop-ball-when-too-slow screen))
           e)) entities))

(defn score-for! [screen player]
  (let [k (if (= player :player-1) :goals-1 :goals-2)
        score (inc (get screen k))
        {:keys [goals-1 goals-2]} (update! screen k score)]
    (screen! text-screen/text-screen :on-goal-scored :goals-1 goals-1 :goals-2 goals-2)))

(defn create-pitch []
  (let [t (texture "Pitch.png")]
    (assoc t :pitch? true)))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (let [screen (update! screen :renderer (stage) :camera (orthographic) :world (box-2d 0 0))]
      (update! screen :goals-1 0 :goals-2 0)
      (width! screen 640)
      [(create-pitch)
       (create-ball screen 75 100)
       (create-lower-bounds screen)
       (create-upper-bounds screen)
       (create-left-bounds screen)
       (create-right-bounds screen)
       (create-left-goalie)
       (create-right-goalie)
       (create-left-goal screen)
       (create-right-goal screen)
       (create-arrow)]))

  :on-begin-contact
  (fn [screen entities]
    (let [f (first-entity screen entities)
          s (second-entity screen entities)]
      (when-let [goal (:goal? f)]
        (score-for! screen (case goal :left :player-1 :player-2))
        nil)))
  
  :on-timer
  (fn [{id :id :as screen} entities])

  :on-touch-down
  (fn [screen entities]
    (map (fn [e]
           (if (:ball? e)
             (let [arrow (find-first :arrow? entities)
                   ball e
                   impulse (:vector arrow)]
               (body! ball :apply-linear-impulse (vector-2! impulse :scl 1000.0 1000.0)
                      (body! ball :get-world-center) true)
               (assoc e :spinning? true))
             e)) entities))

  :on-key-down
  (fn [{:keys [key] :as screen} entities]
    (->> entities
         (map (fn [{:keys [ball?] :as e}]
                (if ball?
                  (cond
                    (= key (key-code :s))
                    (stop-ball! screen e)
                    (= key (key-code :r))
                    (restart-game!)

                    (= key (key-code :num-1))
                    (do
                      (score-for! screen :player-1)
                      e)
                    
                    (= key (key-code :num-2))
                    (do
                      (score-for! screen :player-2)
                      e)

                    :else e) 
                  e)))))
  
  :on-render
  (fn [screen entities]
    (clear!)
    (->> entities
         (step! screen)
         (update-arrow! screen)
         (animate-ball screen)
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
    (set-screen! this main-screen text-screen/text-screen)))

(comment
  (set-screen-wrapper! (fn [screen screen-fn]
                         (try (screen-fn)
                              (catch Exception e
                                (.printStackTrace e)
                                (set-screen! air-soccer-game error-screen)))))
  
  (do
    (require '[air-soccer.core.desktop-launcher :as launcher])
    (launcher/-main))
  


  (require '[play-clj.repl :as repl])
  (filter #(nil? (:body %)) (repl/e main-screen))

  
  ;; RESET TO MAIN SCREEN  
  (on-gl (set-screen! air-soccer-game main-screen text-screen/text-screen))
  )
