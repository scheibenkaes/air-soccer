(ns air-soccer.core.desktop-launcher
  (:require [air-soccer.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. air-soccer-game "air-soccer" 800 600)
  (Keyboard/enableRepeatEvents true))
