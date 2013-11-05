(ns langolier.train
  (:require [langolier.tokenizer :as t])
  (:require [clojure.java.io :as io]))

(def model 
  (atom
   {:n3 {:size 0}
    :n2 {:size 0}
    :n1 {:size 0}
    :lang {:clojure {:n3 {:size 0}
                     :n2 {:size 0}
                     :n1 {:size 0}}
           :java {:n3 {:size 0}
                  :n2 {:size 0}
                  :n1 {:size 0}}}}))

(def DIR "resources/")

(defn- trigrams [tokens]
  "Split tokens into group of three-elements
   [a b c d e] => [[a b c] [b c d] [c d e]]"
  (partition 3 1 tokens))

(defn- bigrams [tokens]
  (partition 2 1 tokens))

(defn- wrap [tokens]
  "Wrap set of tokens into separated structure
   E.g. (:start tok1 tok2 tok3 :end)"
  (concat [:start] tokens [:end]))

;; TODO READABLE CODE WTF!
(defn- fill-ngrams [n-type lang tokens]
  (let [cnt (count tokens) freq (frequencies tokens)]
    (do (swap! model ;; fill global model 
               #(assoc % n-type 
                       (merge-with + 
                                   (assoc freq :size cnt)
                                   (get-in @model [n-type] {})
                                   )))
        (swap! model 
               #(assoc-in % [:lang lang n-type] 
                          (merge-with +
                                      (assoc freq :size cnt)
                                      (get-in @model [:lang lang n-type] {})))))))
        

;; TODO avoid repetition
(defn train [source language]
  (let [tokens (t/tokenize source)]
    (do (fill-ngrams :n3 language (trigrams (wrap tokens)))
        (fill-ngrams :n2 language (bigrams (wrap tokens)))
        (fill-ngrams :n1 language tokens))))

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

;; TODO probability of category
(defn- score [tokens language]
  (reduce +' (map #(log-prob % language) tokens))) 

(defn- scores [tokens languages]
  (map #(vec [% (score tokens %)]) languages))

(defn- best [scores]
  (first (apply max-key second scores)))

(defn classify [source]
  (-> source
      (t/tokenize)
      (trigrams)
      (scores (keys @cat-map))
      (best)))

(defn languages []
  (keys @cat-map))
