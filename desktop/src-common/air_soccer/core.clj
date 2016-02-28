(ns air-soccer.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.math :refer :all]
            [play-clj.g2d-physics :refer :all]
            [play-clj.ui :refer :all]
            [air-soccer.entities :refer :all]
            [air-soccer.screens.text-screen :as text-screen]))

(declare air-soccer-game main-screen)

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
