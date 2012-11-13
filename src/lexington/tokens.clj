(ns ^{ :doc "Lexington Token Representation"
       :author "Yannick Scherer" }
  lexington.tokens)

(defn new-token
  "Create new Token based on a token type and an optional data sequence."
  [type data] 
  (-> {}
    (assoc ::type type)
    (assoc ::data data)
    (assoc ::length (count data))))

(defn token-type
  "Get a Token's type."
  [token]
  (::type token))

(defn token-data
  "Get a Token's data sequence."
  [token]
  (::data token))

(defn token-length
  "Get a Token's data length."
  [token]
  (::length token))
