(ns langolier.train
  (:require [langolier.tokenizer :as t])
  (:require [langolier.validator :as v])
  (:require [clojure.java.io :as io])
  (:require [clojure.set :as s]))

(def model 
  (atom {:global #{}
         :lang {:clojure #{}
                :java #{}}
         :weights {5 500
                   4 300
                   3 50
                   2 10
                   1 1}}))

(def DIR "resources/")

(defn- ngrams [n tokens]
  (partition n 1 tokens))

(defn- wrap [tokens]
  "Wrap set of tokens into separated structure
   E.g. (:start tok1 tok2 tok3 :end)"
  (concat [:start] tokens [:end]))

(defn- model-global []
  (get-in @model [:global] #{}))

(defn- model-lang [lang]
  (get-in @model [:lang lang] #{}))

(defn- weight [n]
  (get-in @model [:weights n] 0))

(defn ngram-present? [ngram lang]
  (contains? (model-lang lang) ngram))

;; TRIE save would be good.
(defn- fill-ngrams [lang ngrams]
  (do 
    (swap! model ;; fill global model 
           #(assoc % :global 
                   (apply conj (model-global) ngrams)))
    (swap! model ;; fill lang specific model
           #(assoc-in % [:lang lang] 
                      (apply conj (model-lang lang) ngrams)))))

(defn train [source language]
  (let [tokens (wrap (t/tokenize source))]
    (v/validate-tokens-count tokens)
    (doseq [i (range 1 6)]
      (fill-ngrams language (ngrams i tokens)))
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
          (train (slurp f) lang-key))))
    :ok))

(defn score-one-ngram [ngram language]
  (cond (empty? ngram) 0
        (ngram-present? ngram language) 
        (weight (count ngram))
        :else (score-one-ngram (butlast ngram) language)))

(defn- score [ngrams language]
  (reduce +' (map #(score-one-ngram % language) ngrams))) 
     
(defn- scores [ngrams languages]
  (map #(vec [% (score ngrams %)]) languages))

(defn- best [scores]
  (first (apply max-key second scores)))

(defn languages []
  (keys (get @model :lang)))

(defn classify [source]
  (v/validate-model model)
  (-> source
      (t/tokenize) ;; at least 3
      (wrap)
      (v/validate-tokens-count)
      (#(ngrams 5 %))
      (scores (languages))
      (#(do (print %) %)) ;; debug
      (best)))
