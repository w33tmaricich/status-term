(ns status-term.display
  (:require [status-term.internet :as i]
            [status-term.git :as g]
            [clj-time.core :as tt]
            [clj-time.coerce :as c]
            [clj-time.format :as f]
            [clj-time.local :as l]
            [lanterna.terminal :as t]
            [lanterna.screen :as s]))

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
  (let [mod-location (if (<= (:w location) 2)
                       {:x (:x location)
                        :y (:y location)
                        :w 3
                        :h (:h location)}
                       location)]
    (fill-color mod-location (:color info))
    (box mod-location)))

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

; All ascii art must have dimensions within this range:
;  width: 10
;  height: 5

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
  ; Put together all the coordinates we will be using.
  (let [; Constants
        icon-w 10
        icon-h 5
        text-h 10
        ; Get information about the weather
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
        coord (:coord forcast)
        ; Put everything together into strings to be written.
        weather-title-str (str "Weather - " city)
        main-descr-str (str " " main-forcast " - " description)
        current-temp-str (str "   Current Temp: "
                              temp-f "\u00b0F/"
                              temp-c "\u00b0C  ")
        high-temp-str    (str "           High: "
                              temp-max-f "\u00b0F/"
                              temp-max-c "\u00b0C")
        low-temp-str     (str "            Low: "
                              temp-min-f "\u00b0F/"
                              temp-min-c "\u00b0C")
        humidity-str (str "   Humidity: " humidity "%")
        sunrise-str (str "   Sunrise: " sunrise "am")
        sunset-str (str "    Sunset: " sunset "pm")
        ; Coordinates
        origin-x (:x location)
        origin-y (:y location)
        origin-w (:w location)
        origin-h (:h location)
        window-w (if (< origin-w (+ 2 (count current-temp-str)))
                   (+ (count current-temp-str) 2)
                   origin-w)
        window-h (if (< origin-h (+ text-h icon-h))
                   (+ text-h icon-h)
                   origin-h)
        center-x (int (/ (+ origin-x window-w) 2))
        center-y (int (/ (+ origin-y window-h) 2))
        text-x (int (- center-x
                       (/ (count current-temp-str) 2)))
        text-y (int (- center-y 
                       (/ text-h 2)))
        icon-x (int (- center-x icon-h))
        icon-y (int (- center-y
                       (/ (+ text-h icon-h) 2)))
        ]
    ; Create the window.
    (window {:x origin-x :y origin-y
             :w window-w :h window-h}
            {:title weather-title-str})
    (let [icon-location {:x icon-x :y icon-y}]
      ; Set the icon depending on the forcast.
      (case main-forcast
        "Clear" (ascii-sunny icon-location)
        "Clouds" (ascii-cloudy icon-location)
        "Fog" (ascii-misty icon-location)
        "Mist" (ascii-misty icon-location)
        "Rain" (ascii-stormy icon-location)
        (ascii-sunny icon-location)))
    ; Display textual information.
    (write text-x (+ 2 text-y) main-descr-str)
    (write text-x (+ 4 text-y) current-temp-str)
    (write text-x (+ 5 text-y) high-temp-str)
    (write text-x (+ 6 text-y) low-temp-str)
    (write text-x (+ 8 text-y) humidity-str)
    (write text-x (+ 10 text-y) sunrise-str)
    (write text-x (+ 11 text-y) sunset-str)))

(defn git-status
  "Returns the current status of a particular git repo."
  [location data]
  (let [; Retrieve important information.
        repo (:repo data)
        path (:path data)
        commits (g/your-commits data)
        weekly-commits (g/weekly-commits data)
        ; Build view text.
        window-title "Commits by Project"
        ]
  (window location {:title window-title})))

(defn create-bar-data-week
  "Creates data needed for the bar graph based on git input"
  [git-input]
  (let [email (:email git-input)
        repo (:repo git-input)
        path (:path git-input)
        commits (g/weekly-commits git-input)]
    {:title (str repo " (" (count commits) ")")
     :val (count commits)
     :color :green}))

(defn git-commits-count-week
  "Displays a count of commits per project within a weekly timeframe."
  [location data]
  (let [projects (into [] (map create-bar-data-week data))
        bar-graph-data {:title "Commits within a week"
                        :items projects}]
    (if (count (:items bar-graph-data))
      (bar-graph location bar-graph-data)
      (window location {:title "No weekly commits to display"}))))


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
    (bar-graph {:x x2 :y y2 :w width :h height}
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
    (git-status {:x x3 :y y3 :w width :h height}
                     {:email "w33tmaricich@gmail.com"
                      :repo "status-term"
                      :path "/home/w33t/code/status-term"})
    (git-commits-count-week {:x x4 :y y4 :w width :h height}
                            [{:email "w33tmaricich@gmail.com"
                              :repo "status-term"
                              :path "/home/w33t/code/status-term"}
                             {:email "w33tmaricich@gmail.com"
                              :repo "posture"
                              :path "/home/w33t/code/posture"}
                             {:email "w33tmaricich@gmail.com"
                              :repo "streammanager"
                              :path "/home/w33t/code/skyline/streammanager"}
                             {:email "w33tmaricich@gmail.com"
                              :repo "dot-py"
                              :path "/home/w33t/code/dot-py"}])
    (refresh)
    (t/get-key-blocking TERM)))
