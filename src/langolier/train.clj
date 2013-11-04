(ns langolier.train
  (:require [langolier.tokenizer :as t])
  (:require [clojure.java.io :as io]))

(def global-map (atom {:default 1}))
(def cat-map (atom {}))
(def DIR "resources/")

(defn train [source language]
  (let [freq (frequencies (t/tokenize source))]
    (swap! global-map #(merge-with + % freq)) ;; add to global map
    (swap! cat-map 
           #(assoc % language 
                   (merge-with + freq (get @cat-map language {}))))
    :ok))

(defn train-dir [dir]
  (let [langs (->> dir
                   (io/file)
                   (.listFiles)
                   (map #(.getName %)))]
    (doseq [lang langs]
      (let [lang-key (keyword lang)
            lang-dir (str dir lang)]
        (doseq [f (remove #(.isDirectory %) 
                          (file-seq (io/file lang-dir)))]
          (train (slurp f) lang-key))))))
  
(defn- log-prob [token language]
  (Math/log
   (/ (double (inc (get (get @cat-map language {}) token 0)))
      (+ (get @global-map token 0) (reduce + (vals @global-map))))))

(defn- score [tokens language]
  (reduce +' (map #(log-prob % language) tokens))) 

(defn- scores [tokens languages]
  (map #(vec [% (score tokens %)]) languages))

(defn- best [scores]
  (first (apply max-key second scores)))

(defn classify [source]
  (-> source
      (t/tokenize)
      (scores (keys @cat-map))
      (best)))

(defn languages []
  (keys @cat-map))
