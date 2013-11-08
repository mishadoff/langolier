(ns langolier.tokenizer)

(def tokens
  (atom 
   {:symbol "\\w+" :sharp "\\#" :percnt "\\%" 
    :period "\\." :comma  "\\," :dollar "\\$"
    :tilda  "\\~" :apostr "\\`" :mathop "[\\+\\-\\*\\/\\=]"
    :exclam "\\!" :questm "\\?" :andor  "[\\&\\|]" 
    :email  "\\@" :quote1 "\\'" :quote2 "\""
    :parenl "\\(" :parenr "\\)" :curlyl "\\[" 
    :squarl "\\{" :squarr "\\}" :curlyr "\\]"
    :colon  "\\:" :scolon "\\;" :undscr "\\_"
    :arrow1 "\\^" :arrow2 "\\<" :arrow3 "\\>"}))

;; TODO move wrapper here

(defn- build-regexp []
  (->> @tokens
       vals
       (interpose "|")
       (apply str)
       (re-pattern)))

(defn- transform [f tokens]
  (map f tokens))

(defn- apply-filters [tokens]
  (->> tokens
       ;; replace numbers to "<number>"
       (transform #(if (re-matches #"\d+" %) "<number>" %))
       ;; one-letter to "<ident>" 
       (transform #(if (re-matches #"\w" %) "<ident>" %))))

(defn tokenize [source]
  (->> source
       (re-seq (build-regexp))
       (apply-filters)))
