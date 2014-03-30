(ns langolier.train
  (:require [langolier.tokenizer :as t])
  (:require [clj-http.client :as http])
  (:require [clojure.data.json :as json]))


#_(http/get "https://api.github.com/search/code?q=a+in:file+language:clojure+user:clojure")


