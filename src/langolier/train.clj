(ns langolier.train
  (:require [langolier.tokenizer :as t]))

(def global-map (atom {:default 1}))
(def cat-map (atom {}))

(defn train [source language]
  (let [freq (frequencies (t/tokenize source))]
    (swap! global-map #(merge-with + % freq)) ;; add to global map
    (swap! cat-map 
           #(assoc % language 
                   (merge-with + freq (get @cat-map language {}))))
    :ok))

(defn log-prob [token language]
  (Math/log
   (/ (double (inc (get (get @cat-map language {}) token 0)))
      (+ (get @global-map token 0) (reduce + (vals @global-map))))))

(defn score [tokens language]
  (reduce +' (map #(log-prob % language) tokens))) 

(defn scores [tokens languages]
  (map #(vec [% (score tokens %)]) languages))

(defn best [scores]
  (first (apply max-key second scores)))

(defn classify [source]
  (-> source
      (t/tokenize)
      (scores (keys @cat-map))
      (best)))
