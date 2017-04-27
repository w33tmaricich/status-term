(ns status-term.internet
  (:require [clojure.data.json :as json]
            [clj-http.client :as client]))

(defn get-weather
  "Gets the weather from online using your zip code."
  [zip]
  (let [api-key (slurp "openweathermap.apikey")
        response (client/get (str "http://api.openweathermap.org/data/2.5/weather?zip="
                                  zip
                                  "&appid="
                                  api-key))]
    (if (= 200 (:status response))
      (let [data (:body response) ;string
            body (json/read-str data :key-fn keyword)]
        body)
      nil)))
