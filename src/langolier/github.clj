(ns langolier.github
  (:require [clj-http.client :as http])
  (:require [clojure.data.json :as json]))

(defn get-raw-links [page]
  "return list of pairs [language raw_url] from gist page"
  (->> (str "https://api.github.com/gists?page=" page)
       (http/get)
       (:body)
       (json/read-str)
       (map #(get % "files"))
       (mapcat (fn [m]
                 (map (fn [[k v]]
                        [(get v "language")
                         (get v "raw_url")]) m)))))


(defn process []
  (reduce #(apply conj %1 (get-raw-links %2))
          []
          (range 1 100)))
