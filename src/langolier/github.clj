(ns langolier.github
  (:require [clj-http.client :as http])
  (:require [clojure.data.json :as json]))

;; TODO Could be a part of buldozer

;; set this variables or provide bindings
(def ^:dynamic *username* nil)
(def ^:dynamic *password* nil)

(defn- auth-map []
  (if (and *username*
           *password*) {:basic-auth
                        [*username* *password*]}
           {}))

(defn get-raw-links [page]
  "return list of pairs [language raw_url] from gist page"
  (->> (str "https://api.github.com/gists/public?page=" page)
       (#(http/get % (auth-map)))
       (:body)
       (json/read-str) ;; TODO key-fn
       (map #(get % "files"))
       (mapcat (fn [m]
                 (map (fn [[k v]]
                        [(get v "language")
                         (get v "raw_url")]) m)))))


(defn process []
  (reduce #(apply conj %1 (get-raw-links %2))
          [] (range 1 100)))
   
   
