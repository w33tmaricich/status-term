(ns status-term.core
  (:require [status-term.display :as display]
            [lanterna.terminal :as t]
            [lanterna.screen :as s])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;(display/develop args)
  ;(display/start)
  ;(display/fill-color 0 0 10 10 :green))
  (display/develop args))
  ;(s/stop SCREEN)
  ;(t/stop TERM))
