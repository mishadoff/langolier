(ns langolier.validator)

(defn validate-tokens-count [tokens]
  (if (< (count tokens) 5)
    (throw (IllegalArgumentException. 
            "Source must contain at least 3 tokens"))
    tokens))

(defn validate-model [model]
  (if (zero? (count (:global @model)))
    (throw (IllegalArgumentException.
            "Model is empty"))
    model))
