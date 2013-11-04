(ns langolier.tokenizer)

(def tokens
  (atom 
   {:symbol "\\w+" :sharp "\\#" :percnt "\\%" 
    :period "\\." :comma_ "\\," :dollar "\\$"
    :tilda_ "\\~" :apostr "\\`" :mathop "[\\+\\-\\*\\/\\=]"
    :exclam "\\!" :questm "\\?" :andor_ "[\\&\\|]" 
    :email_ "\\@" :quote1 "\\'" :quote2 "\""
    :parenl "\\(" :parenr "\\)" :curlyl "\\[" 
    :squarl "\\{" :squarr "\\}" :curlyr "\\]"
    :colon_ "\\:" :scolon "\\;" :undscr "\\_"
    :arrow1 "\\^" :arrow2 "\\<" :arrow3 "\\>"}))

(defn- build-regexp []
  (->> @tokens
       vals
       (interpose "|")
       (apply str)
       (re-pattern)))

(defn tokenize [source]
  (re-seq (build-regexp) source))
