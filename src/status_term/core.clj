(ns status-term.core
  (:require [lanterna.terminal :as t]
            [lanterna.screen :as s])
  (:gen-class))

(def TERM (t/get-terminal :text))
(def SCREEN (s/get-screen :text))

(defn write 
  "Write some formatted text to the terminal."
  [x y text]
  (s/put-string SCREEN x y text))

(defn write-vertical
  "Writes some text virtically"
  ([x y text]
   (write-vertical x y text 0))
  ([x y text index]
   (if (empty? text)
     :done
     (let [f-char (first text)
           rest-string (rest text)]
       (write x
              (+ y index)
              (str f-char))
       (write-vertical x y rest-string (inc index))))))

(defn border
  "Creates a square."
  [x y height width character]
  (if (not (map? character))
    (let [location-map {:top character
                        :bottom character
                        :left character
                        :right character}]
      (border x y height width location-map))
    (let [border-height (inc height)
          border-width (inc width)
          top-border (apply str (repeat border-height (:top character)))
          bottom-border (apply str (repeat border-height (:bottom character)))
          left-border (apply str (repeat border-width (:left character)))
          right-border (apply str (repeat border-width (:right character)))]
      (write x y top-border)
      (write x (+ y height) bottom-border)
      (write-vertical x y left-border)
      (write-vertical (+ x width) y right-border))))

(defn window
  "A stylized window with content."
  []
  (let [size (t/get-size TERM)]
    (border 0 0 30 30 "#")
    (border 15 15 30 30 {:top "-"
                         :bottom "_"
                         :left "|"
                         :right "+"})
    (s/redraw SCREEN)))

(defn develop [args]
  (window))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (t/start TERM)
  (s/start SCREEN)
  (develop args)
  (Thread/sleep 5000))
  ;(develop args)
  ;(s/stop SCREEN)
  ;(t/stop TERM))
