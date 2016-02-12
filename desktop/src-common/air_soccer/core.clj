(ns air-soccer.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.math :refer :all]
            [play-clj.g2d-physics :refer :all]
            [play-clj.ui :refer :all]))

(declare air-soccer-game main-screen text-screen)

(def ^:const goal-size 64)
(def ^:const border-strength 5)

(def ^:const font-32 "Fipps-32.fnt")
(def ^:const font-16 "Fipps-16.fnt")

(defn create-arrow []
  (let [t (texture "Arrow.png")
        width (texture! t :get-region-width)
        height (texture! t :get-region-height)]
    (assoc t :arrow? true
           :x 36 :y 64 :width width :height height)))

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

(defn create-score-indicator []
  (let [x (/ (game :width) 2)
        y (- (game :height) 35)
        l (label "0:0" (style :label (style :label (bitmap-font font-16) (color :white)))
                 :set-alignment (align :center))]
    (assoc l
           :scoreboard? true
           :goals-1 0 :goals-2 0
           :x (- x (/ (label! l :get-pref-width) 2))
           :y y)))

(defn create-goal-scored-text []
  (let [x (/ (game :width) 2)
        y (/ (game :height) 2)
        l (label "GOAL!!!" (style :label (bitmap-font font-32) (color :white))
                 :set-alignment (align :center))
        x (- x (/ (label! l :get-pref-width) 2))]
    (assoc l
           :label/goal-scored? true :x x :y y)))

(defscreen text-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    [(assoc (label "0" (color :red))
            :fps? true
            :x 5 :y 0)
     (create-score-indicator)])

  :on-timer
  (fn [screen entities]
    (when (= :remove-goal-indicator (:id screen))
      (remove :label/goal-scored? entities)))
  
  :on-goal-scored
  (fn [{:keys [goals-1 goals-2] :as screen} entities]
    (add-timer! screen :remove-goal-indicator 3)
    (->> entities
     (map (fn [e]
            (if (:scoreboard? e)
              (assoc e :goals-1 goals-1 :goals-2 goals-2)
              e)))
     (cons (create-goal-scored-text))))
  
  :on-render
  (fn [screen entities]
    (letfn [(render-fps [entities]
              (map (fn [e]
                     (if (:fps? e)
                       (doto e
                         (label! :set-text (str (game :fps))))
                       e)) entities))
            (render-scoreboard [entities]
              (map (fn [e]
                     (if (:scoreboard? e)
                       (let [{:keys [goals-1 goals-2] :or {goals-1 0
                                                           goals-2 0}} e
                             s (str goals-1 ":" goals-2)]
                         (doto e (label! :set-text s)))
                       e)) entities))]
      (->> entities
           (render-fps)
           (render-scoreboard)
           (render! screen)))))

(defn restart-game! []
  (on-gl (set-screen! air-soccer-game main-screen text-screen)))

(defn animate-ball [screen entities]
  (map (fn [{:keys [animation ball? spinning?] :as e}]
         (if ball?
           (if spinning?
            (let [t (animation->texture screen animation)]
              (merge e t))
            e)
           e)) entities))

(defn score-for! [screen player]
  (let [k (if (= player :player-1) :goals-1 :goals-2)
        score (inc (get screen k))
        {:keys [goals-1 goals-2]} (update! screen k score)]
    (screen! text-screen :on-goal-scored :goals-1 goals-1 :goals-2 goals-2)))

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
       (create-arrow)]))

  :on-touch-down
  (fn [screen entities]
    (let [arrow (find-first :arrow? entities)
          ball (find-first :ball? entities)
          impulse (:vector arrow)]
      (body! ball :apply-linear-impulse (vector-2! impulse :scl 1000.0 1000.0)
             (body! ball :get-world-center) true))
    nil)

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
  (on-gl (set-screen! air-soccer-game main-screen text-screen))

  (require '[play-clj.repl :as repl])
  (repl/e main-screen)

  )
