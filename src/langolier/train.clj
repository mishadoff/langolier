(ns langolier.train
  (:require [clojure.string :as s]))

(def global-map (atom {}))
(def cat-map (atom {}))

(defn tokenize [source]
  "Split source by space separator"
  (s/split source #" "))

(defn train [source language]
  (let [freq (frequencies (tokenize source))]
    (swap! global-map #(merge-with + % freq)) ;; add to global map
    (swap! cat-map 
           #(assoc % language 
                   (merge-with + freq (get @cat-map language {}))))))

;; TODO add laplasian smoothing
;; TODO add logprob
(defn prob [token language]
  (/ (double (get (get cat-map language) token 0))
     (get global-map token 1)))

(defn score [tokens language]
  (reduce *' (map #(prob % language) tokens))) 

(defn scores [tokens languages]
  (map #(vec [% (score tokens %)]) languages))

(defn best [scores]
  (first (apply max-key second scores)))

(defn classify [source]
  (-> source
      (tokenize)
      (scores [:java :clojure])
      (best)))
