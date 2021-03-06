(ns air-soccer.screens.text-screen
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.ui :refer :all]
            [air-soccer.entities :refer :all]))

(def ^:const font-32 "Fipps-32.fnt")
(def ^:const font-16 "Fipps-16.fnt")

(defn create-score-indicator []
  (let [x (/ (game :width) 2)
        y (- (game :height) 35)
        l (label "0:0" (style :label (style :label (bitmap-font font-16) color-1))
                 :set-alignment (align :center))]
    (assoc l
           :scoreboard? true
           :goals-1 0 :goals-2 0
           :x (- x (/ (label! l :get-pref-width) 2))
           :y y)))

(defn create-goal-scored-text []
  (let [l (label "GOAL!!!" (style :label (bitmap-font font-32) color-1)
                 :set-alignment (align :center))
        x (- (/ (game :width) 2)
             (/ (label! l :get-pref-width) 2))
        y (+ (/ (game :height) 2)
             (label! l :get-pref-height))]
    (assoc l :label/goal-scored? true :x x :y y)))

(defn- create-player-indicator []
  (let [l (label "TBD" (style :label (bitmap-font font-16) color-1))]
    (assoc l :label/player-indicator? true :x 10 :y (- (game :height) 35))))

(defn update-player-indicator [current-player entities]
  (let [l (find-first :label/player-indicator? entities)]
    (label! l :set-text (str current-player)))
  nil)

(defscreen text-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    [(assoc (label "0" color-4)
            :fps? true
            :x 5 :y 0)
     (create-score-indicator)
     (create-player-indicator)])

  :on-timer
  (fn [screen entities]
    (when (= :remove-goal-indicator (:id screen))
      (remove :label/goal-scored? entities)))

  :on-player-change
  (fn [screen entities]
    (let [current-player (:current-player screen)]
      (update-player-indicator current-player entities)))
  
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
