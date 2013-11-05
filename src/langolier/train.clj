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
  (let [rm (-> tokens
               (frequencies)
               (assoc :size (count tokens)))]
    (do (swap! model ;; fill global model 
               #(assoc % n-type 
                       (merge-with + rm (get-in @model [n-type] {})
                                   )))
        (swap! model ;; fill lang specific model
               #(assoc-in % [:lang lang n-type] 
                          (merge-with + rm (get-in @model [:lang lang n-type] {})))))))
        

;; TODO avoid repetition
(defn train [source language]
  (let [tokens (t/tokenize source)]
    (do (fill-ngrams :n3 language (trigrams (wrap tokens)))
        (fill-ngrams :n2 language (bigrams (wrap tokens)))
        (fill-ngrams :n1 language tokens)
        :ok)))

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

;; TODO make abstractions like dictsize, tokencat instead of get-in..
(defn unigram-probability [token language]
  (let [a (get-in @model [:lang language :n1 token] 0)
        b (get-in @model [:n1 token] 0)] ;; to aviod div/0
    (/ (double (inc a))
       (+ b (get-in @model [:n1 :size]))))) ;; dict size
          

(defn- probability [token language]
  (let [trigram (get-in @model [:lang language :n3 token] 0)]
    (cond (> trigram 0) 
          (/ (double trigram) (get-in @model [:n3 token]))
          :else ;; backoff strategy
          (let [bigram (get-in @model [:lang language :n2 (butlast token)] 0)]
            (cond (> bigram 0)
                  (* (/ (double bigram) (get-in @model [:n2 (butlast token)]))
                     (unigram-probability (last token) language))
                  :else 
                  (reduce *' (map #(unigram-probability % language) token)))))))

(defn- lang-probability [lang]
  (let [total (get-in @model [:n1 :size])]
    (if (zero? total) 
      (throw (IllegalArgumentException. "Model is empty"))
      (/ (double (get-in @model [:lang lang :n1 :size] 0))
         total))))

(defn- score [tokens language]
  (reduce +'
          ;;(Math/log (lang-probability language))
          (map #(Math/log (probability % language)) tokens)))

(defn- scores [tokens languages]
  (map #(vec [% (score tokens %)]) languages))

(defn- best [scores]
  (first (apply max-key second scores)))

(defn classify [source]
  (-> source
      (t/tokenize) ;; at least 1
      (wrap)
      (trigrams)
      (scores (languages))
      (best)))

(defn languages []
  (keys (get @model :lang)))
