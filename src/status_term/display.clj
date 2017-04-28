(ns status-term.display
  (:require [status-term.internet :as i]
            [clj-time.core :as tt]
            [clj-time.coerce :as c]
            [clj-time.format :as f]
            [clj-time.local :as l]
            [lanterna.terminal :as t]
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
        ys (range y (+ y height))]
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
  (let [title (str (:title info) " ")
        title-length (count title)
        center-x (:x location)
        center-y (inc (:y location))
        seperator (repeat-string THIN-VERTICAL-LINE 3)
        scale 1
        bar-x (+ center-x 2)
        bar-y center-y
        bar-w (* scale (:val info))
        title-x (- center-x title-length)
        title-y (+ 1 center-y)]
    (write title-x title-y title)
    (write-vertical center-x
                    center-y
                    seperator)
    (bar {:x bar-x :y bar-y :w bar-w :h 3}
         {:color (:color info)})))

(defn title-width
  "Calculates the total width of a bargraph row."
  [row]
  (+ (count (:title row))
     3))

(defn bar-width
  [row]
  (+ (:val row)
     3))

(defn largest-title
  "Finds the max title-width of a list of bargraphs"
  [rows]
  (let [sizes (map title-width rows)]
    (apply max sizes)))

(defn largest-bar
  [rows]
  (let [sizes (map bar-width rows)]
    (apply max sizes)))

(defn bar-graph
  "A bar graph widget."
  [location data]
  (let [title (:title data)
        bars (:items data)
        t-width (largest-title bars)
        bar-width (largest-bar bars)
        graph-width (+ t-width bar-width)
        origin-x (:x location)
        origin-y (:y location)
        width (if (< (:w location) graph-width)
                graph-width
                (:w location))
        graph-height (* 3 (count bars))
        height (if (< (:h location) (+ 2 graph-height))
                 (+ 2 graph-height)
                 (:h location))
        max-x (+ origin-x width)
        max-y (+ origin-y height)
        center-x (+ origin-x (/ width 2))
        center-y (+ origin-y (/ height 2))
        bar-x (+ t-width origin-x)
        bar-y (- center-y (/ graph-height 2))]
    (window {:x origin-x
             :y origin-y
             :w width
             :h height}
            {:title title})
    (doall (map-indexed #(bargraph-row {:x bar-x
                                 :y (+ bar-y (* %1 3))
                                 :w width :h height}
                                %2)
                        bars))))

(defn k->f
  "Converts a temperature from kelvin to fahrenheight"
  [k]
  (int (- (* k (/ 9 5)) 459.67)))

(defn k->c
  "Converts a temperature from kelvin to celsius"
  [k]
  (int (- k 273.15)))

(defn format-time
  [unformatted-time]
  (let [joda (l/to-local-date-time (c/from-long unformatted-time))
        hour (tt/hour joda)
        minute (tt/minute joda)]
    (str hour ":" minute)))


(defn ascii-cloudy
  "Draws an ascii cloud."
  [location]
  (let [x (:x location)
        y (:y location)]
    (write (+ 3 x) (inc y)     "___"       )
    (write (+ 2 x) (+ 2 y)    "(   )"      )
    (write x (+ 3 y)        "(___)__)"     )))

(defn ascii-misty
  "Draws ascii mist."
  [location]
  (let [x (:x location)
        y (:y location)]
    (write (+ 2 x) (inc y)  "--")
    (write (+ 1 x) (+ 2 y) "-------___")
    (write (+ 3 x) (+ 3 y)   "____---")))

(defn ascii-stormy
  "A stormy cloud!"
  [location]
  (let [x (:x location)
        y (:y location)]
    (ascii-cloudy {:x x :y (dec y)})
    (write (inc x) (+ 3 y)   "` `_/`"   )
    (write (+ 2 x) (+ 4 y)          "`/ ` `"  )))

(defn ascii-sunny
  "Draws an ascii art sun!"
  [location]
  (let [x (:x location)
        y (:y location)]
    (write (+ 4 x) y            "|"      )
    (write (+ 2 x) (+ 1 y)   "\\ _ /"    )
    (write x (+ 2 y)        "-= (_) =-"  )
    (write (+ 2 x) (+ 3 y)    "/   \\"   )
    (write (+ 4 x) (+ 4 y)      "|"      )))

(defn weather
  [location data]
  (let [origin-x (inc (:x location))
        origin-y (+ 6 (:y location))
        origin-w (:w location)
        origin-h (:h location)
        zip (:zip data)
        forcast (i/get-weather zip)
        weather (first (:weather forcast))
        main-forcast (:main weather)
        description (:description weather)
        city (:name forcast)
        clouds (:clouds forcast)
        sys (:sys forcast)
        sunrise (format-time (:sunrise sys))
        sunset (format-time (:sunset sys))
        base (:base forcast)
        main (:main forcast)
        temp-k (:temp main)
        temp-f (k->f temp-k)
        temp-c (k->c temp-k)
        temp-min-k (:temp_min main)
        temp-min-f (k->f temp-k)
        temp-min-c (k->c temp-k)
        temp-max-k (:temp_max main)
        temp-max-f (k->f temp-max-k)
        temp-max-c (k->c temp-max-k)
        humidity (:humidity main)
        visibility (:visibility forcast)
        coord (:coord forcast)]
    ; Create the window.
    (window location {:title (str "Weather - " city)})
    (let [icon-x (+ 4 (:x location))
          icon-y (+ 2 (:y location))
          icon-location {:x icon-x :y icon-y}]
      ; Set the icon depending on the forcast.
      (case main-forcast
        "Clear" (ascii-sunny icon-location)
        "Clouds" (ascii-cloudy icon-location)
        "Fog" (ascii-misty icon-location)
        "Mist" (ascii-misty icon-location)
        "Rain" (ascii-stormy icon-location)
        (ascii-sunny icon-location)))
    ; Display textual information.
    (write (inc origin-x) (+ 2 origin-y) (str " " main-forcast " - " description))
    (write (inc origin-x) (+ 4 origin-y) (str "   Current Temp: "
                                              temp-f "\u00b0F/"
                                              temp-c "\u00b0C"))
    (write (inc origin-x) (+ 5 origin-y) (str "           High: "
                                              temp-max-f "\u00b0F/"
                                              temp-max-c "\u00b0C"))
    (write (inc origin-x) (+ 6 origin-y) (str "            Low: "
                                              temp-min-f "\u00b0F/"
                                              temp-min-c "\u00b0C"))
    (write (inc origin-x) (+ 8 origin-y) (str "   Humidity: " humidity "%"))
    (write (inc origin-x) (+ 10 origin-y) (str "   Sunrise: " sunrise "am"))
    (write (inc origin-x) (+ 11 origin-y) (str "    Sunset: " sunset "pm"))))

(defn develop
  "Some basic tests of the systems."
  [args]
  (start)
  (let [window-size (t/get-size TERM)
        window-max [(dec (first window-size))
                    (dec (second window-size))]
        width (/ (first window-max) 2)
        height (/ (second window-max) 2)
        x1 0 y1 0
        x2 (inc width) y2 y1
        x3 x1 y3 (inc height)
        x4 (inc width) y4 (inc height)]
    ;(fill-color {:x 10 :y 1 :w 20 :h 3} :red)
    ;(box {:x 0 :y 0 :h 40 :w 20})
    ;(bar {:x 10 :y 10 :w 20 :h 3}
         ;{:color :green})
    ;(window {:x 0 :y 0 :w (first window-max) :h (second window-max)}
            ;{:title "Develop"})
    ;(bargraph-row {:x 10 :y 0 :w 20 :h 20}
                  ;{:title "2017-04-23 (4)"
                   ;:val 5
                   ;:color :green})
    (bar-graph {:x x3 :y y3 :w width :h height}
               {:title "Bar graph test - Years"
                :items [{:title "Alex's age"
                         :val 24
                         :color :yellow}
                        {:title "Lucia's age"
                         :val 23
                         :color :blue}
                        {:title "Programming years"
                         :val 5
                         :color :red}]})
    (weather {:x x1 :y y1 :w width :h height}
             {:zip "21061,us"})
    (refresh)
    (t/get-key-blocking TERM)))
