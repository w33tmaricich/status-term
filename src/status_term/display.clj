(ns status-term.display
  (:require [lanterna.terminal :as t]
            [lanterna.screen :as s]))

;(defstruct s-LOCATION

; Grab the terminal we are writting to.
(def TERM (t/get-terminal :text))
(def SCREEN (s/get-screen :text))
(def THIN-HORIZONTAL-LINE "\u2500")
(def THIN-VERTICAL-LINE "\u2502")
(def THIN-TOP-LEFT "\u250c")
(def THIN-TOP-RIGHT "\u2510")
(def THIN-BOTTOM-LEFT "\u2514")
(def THIN-BOTTOM-RIGHT "\u2518")

(defn start
  "starts the terminal and screen session connections."
  []
  (t/start TERM)
  (s/start SCREEN))

; Refresh the screen.
(defn refresh [] (s/redraw SCREEN))

; Make sure when we write we are using put-string on screen.
(def write (partial s/put-string SCREEN))

(defn write-vertical
  "Performs the same task as write but writes text vertically."
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

(defn repeat-string
  "Repeats a character to create a string."
  [character length]
  (apply str (repeat length character)))

(defn box
  "Draws a unicode box."
  [data]
  (let [x (:x data)
        y (:y data)
        width (:w data)
        height (:h data)
        tb-string-size (- width 2)
        lr-string-size (- height 2)
        tb-string (repeat-string THIN-HORIZONTAL-LINE tb-string-size)
        lr-string (repeat-string THIN-VERTICAL-LINE lr-string-size)
        min-x (inc x)
        min-y (inc y)
        max-x (+ (dec width) x)
        max-y (+ (dec height) y)]
    ; angled top left corner
    (write x y THIN-TOP-LEFT)
    ; top line
    (write min-x y tb-string)
    ; angled top right corner
    (write max-x y THIN-TOP-RIGHT)
    ; left horizontal line
    (write-vertical x min-y lr-string)
    ; right horizontal line
    (write-vertical max-x min-y lr-string)
    ; left bottom corner
    (write x max-y THIN-BOTTOM-LEFT)
    ; bottom horizontal line.
    (write min-x max-y tb-string)
    ; right bottom corner
    (write max-x max-y THIN-BOTTOM-RIGHT)))

(defn fill-color
  "Fill a color into an area."
  [location color]
  (let [x (:x location)
        y (:y location)
        width (:w location)
        height (:h location)
        spaces (repeat-string " " width)
        ys (range y height)]
    (dorun (map #(write x % spaces {:bg color}) ys))))

(defn window
  "A box that also has a title"
  [location info]
  (fill-color location (:color info))
  (box location)
  (write (+ (:x location) 2) (:y location) (:title info)))

(defn bar
  "bar for bar graph."
  [location info]
  (fill-color location (:color info))
  (box location))

(defn bargraph-row
  "a row of a bar graph"
  [location info]
  (let [center-x (:x location)
        center-y (inc (:y location))
        seperator (repeat-string THIN-VERTICAL-LINE 3)
        bar-x (+ center-x 2)
        bar-y center-y
        title (:title info)
        title-length (count title)
        title-x (- center-x title-length)
        title-y center-y]
    (write-vertical center-x
                    center-y
                    seperator)
    (bar {:x bar-x :y bar-y :w (:w location) :h 3}
         {:color (:color info)})))

(defn develop
  "Some basic tests of the systems."
  [args]
  (start)
  (let [window-size (t/get-size TERM)
        window-max [(dec (first window-size))
                    (dec (second window-size))]]
    ;(bar {:x 0 :y 0 :w 20 :h 3}
         ;{:color :green})
    ;(box {:x 0 :y 0 :h 40 :w 20})
    ;(window {:x 0 :y 0 :w (first window-max) :h (second window-max)}
            ;{:title "Develop"})
    (bargraph-row {:x 0 :y 0 :w 20 :h 20}
                  {:title "bar 1"
                   :val 4
                   :color :green})
;    (bar-graph 0 0 (first window-max) (second window-max)
               ;{:title "Good old bar graph"
                ;:items {:title "fun"
                        ;:val 3
                        ;:color :red}})
    (refresh)
    (t/get-key-blocking TERM)))
