(ns air-soccer.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.math :refer :all]
            [play-clj.g2d-physics :refer :all]
            [play-clj.ui :refer :all]
            [air-soccer.entities :refer :all]
            [air-soccer.screens.text-screen :as text-screen]))

(declare air-soccer-game main-screen)

1(defn play-sound
  "Plays a single sound via its id"
  [sound-id]
  (let [sample (case sound-id
                 :goal (sound "sounds/goal.ogg"))]
    (sound! sample :play)))

(defn set-current-player! [screen player]
  (update! screen :current-player player))

(defn stop-ball-body! [ball]
  (doto ball
    (body! :set-linear-velocity (vector-2 0 0))))

(defn stop-ball! [screen ball & {:keys [current-player]
                                 :or {current-player :player-1}}]
  (set-current-player! screen current-player)
  (screen! text-screen/text-screen :on-player-change :current-player current-player)
  (-> (stop-ball-body! ball)
      (assoc :spinning? false)))

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


(defn- animate-spinning-ball [{:keys [current-player] :as screen} {:keys [animation tiles spinning?] :as e}]
  (if spinning?
    (let [t (animation->texture screen animation)]
      (merge e t))
    (let [idx (case current-player :player-1 0 1)]
      (merge e (nth tiles idx)))))

(def ^:const stop-ball-at "Length of a vector" 1600)

(defn ball-too-slow?
  "When the ball is getting so slow we want to stop it."
  [v]
  (-> (vector-2! v :len2) (< stop-ball-at)))

(defn rand-player []
  (rand-nth [:player-1 :player-2]))

(defn- stop-ball-when-too-slow [screen {:keys [spinning?] :as e}]
  (let [speed (body! e :get-linear-velocity)]
    (if (and spinning?
             (not= (vector-2 0 0) speed)
             (ball-too-slow? speed))
      [(stop-ball! screen e :current-player (rand-player)) (create-arrow)]
      e)))

(defn animate-ball [screen entities]
  (map (fn [{:keys [ball? spinning?] :as e}]
         (if ball?
           (->> e (animate-spinning-ball screen) (stop-ball-when-too-slow screen))
           e)) entities))

(defn score-for! [{:keys [player] :as screen}]
  (let [k (if (= player :player-1) :goals-1 :goals-2)
        score (inc (get screen k))
        {:keys [goals-1 goals-2]} (update! screen k score)]
    (play-sound :goal)
    (screen! text-screen/text-screen :on-goal-scored :goals-1 goals-1 :goals-2 goals-2)))

(defn center-ball
  "must not be called from a collission detection fn, 
  as the world is locked at this moment and setting a position will fail
  at this point"
  [screen entities]
  (map (fn [{:keys [ball? width height angle] :as e}]
         (if ball?
           (let [x (- (center-x)
                      (/ width 2))
                 y (- (center-y)
                      (/ height 2))]
             (doto e
               (body-position! x y (:angle e))))
           e)) entities))

(defn other-guy [current]
  (case current
    :player-1 :player-2 :player-1))

(defn on-goal-scoring
  "Accumulates whatever should be done when a goal is scored.
  :player in `screen` should indicate who scored"
  [screen entities]
  (when-let [player (:player screen)]
    ;; needed as box2d does not like changing a body's position
    ;; while the world is locked
    (add-timer! screen :after-goal 0.01)
    (conj (mapv (fn [{:keys [ball?] :as e}]
                  (if ball?
                    (do
                      (score-for! screen)
                      (stop-ball! screen e :current-player (other-guy player)))
                    e)) entities)
          (create-arrow))))

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
        (on-goal-scoring
         (assoc screen :player (case goal :left :player-2 :player-1))
         entities))))
  
  :on-timer
  (fn [{id :id :as screen} entities]
    (when (= id :after-goal)
      (->> entities
           (center-ball screen))))

  :on-touch-down
  (fn [screen entities]
    (when-let [arrow (find-first :arrow? entities)]
      (->> entities
           (mapv (fn [e]
                   (if (:ball? e)
                     (let [ball e
                           impulse (:vector arrow)]
                       (body! ball :apply-linear-impulse (vector-2! impulse :scl 1000.0 1000.0)
                              (body! ball :get-world-center) true)
                       (assoc e :spinning? true))
                     e)))
           (remove :arrow?))))

  :on-key-down
  (fn [{:keys [key] :as screen} entities]
    (->> entities
         (mapv (fn [{:keys [ball?] :as e}]
                 (if ball?
                   (cond
                     (= key (key-code :s))
                     (do
                       [(stop-ball! screen e :current-player (rand-player)) (create-arrow)])
                     (= key (key-code :r))
                     (restart-game!)

                     (= key (key-code :num-1))
                     (do
                       (score-for! (assoc screen :player :player-1))
                       e)
                     
                     (= key (key-code :num-2))
                     (do
                       (score-for! (assoc screen :player :player-2))
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
         flatten
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

  (-> (repl/s main-screen) :world .isLocked)

  
  ;; RESET TO MAIN SCREEN  
  (on-gl (set-screen! air-soccer-game main-screen text-screen/text-screen))
  )
