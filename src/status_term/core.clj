(ns status-term.core
  (:require [status-term.display :as display]
            [lanterna.terminal :as t]
            [lanterna.screen :as s])
  (:gen-class))

;(defn write-vertical
  ;"Writes some text virtically"
  ;([x y text]
   ;(write-vertical x y text 0))
  ;([x y text index]
   ;(if (empty? text)
     ;:done
     ;(let [f-char (first text)
           ;rest-string (rest text)]
       ;(write x
              ;(+ y index)
              ;(str f-char))
       ;(write-vertical x y rest-string (inc index))))))

;(defn border
  ;"Creates a square."
  ;[x y width height character]
  ;(if (not (map? character))
    ;(let [location-map {:top character
                        ;:bottom character
                        ;:left character
                        ;:right character}]
      ;(border x y height width location-map))
    ;(let [border-height (inc height)
          ;border-width (inc width)
          ;top-border (apply str (repeat border-height (:top character)))
          ;bottom-border (apply str (repeat border-height (:bottom character)))
          ;left-border (apply str (repeat border-width (:left character)))
          ;right-border (apply str (repeat border-width (:right character)))]
      ;(write x y top-border)
      ;(write x (+ y height) bottom-border)
      ;(write-vertical x y left-border)
      ;(write-vertical (+ x width) y right-border))))

;(defn border-title
  ;"Places some text on a border."
  ;[x y text]
  ;(write (+ x 3)
         ;y
         ;text))

;(defn background
  ;"Creates an area that has a background with a given color."
  ;[x y width height color]
  ;(t/set-bg-color TERM color))

;(defn window
  ;"A stylized window with content."
  ;[]
  ;(let [size (t/get-size TERM)]
    ;(t/set-fg-color TERM :green)
    ;(write 10 10 (str size))
    ;(border 0 0 30 30 "#")
    ;(border-title 0 0 "Border 1")
    ;(border 15 15 30 30 {:top "-"
                         ;:bottom "_"
                         ;:left "|"
                         ;:right "+"})
    ;(border-title 15 15 "Border 2")
    ;(s/redraw SCREEN)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;(display/develop args)
  ;(display/start)
  ;(display/fill-color 0 0 10 10 :green))
  (display/develop args))
  ;(s/stop SCREEN)
  ;(t/stop TERM))
