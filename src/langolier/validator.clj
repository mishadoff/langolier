(ns langolier.validator)

(defn validate-tokens-count [tokens]
  (if (< (count tokens) 5)
    (throw (IllegalArgumentException. 
            "Source must contain at least 3 tokens"))))
