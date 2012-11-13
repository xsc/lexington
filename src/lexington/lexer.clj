(ns ^{ :doc "Lexington Lexer Implementation.
             A lexer is a function that takes a sequence of input entities
             (e.g. characters) and produces a sequence of tokens."
       :author "Yannick Scherer" }
  lexington.lexer
  (:use lexington.tokens
        lexington.token-readers))

;; ---------------------------------------------------------------------
;; General Lexer Structure
;; ---------------------------------------------------------------------
(defn- run-token-readers
  "Run a sequence of token readers in order until one produces a non-nil value."
  [token-readers s]
  (loop [r token-readers]
    (when (seq r)
      (if-let [token ((first r) s)]
        token
        (recur (rest r))))))

(defn- run-lexer
  "Run a sequence of token readers on a given input sequence, producing a lazy
   seq with the read tokens."
  [token-readers s]
  (lazy-seq
    (when (seq s)
      (when-let [token (run-token-readers token-readers s)]
        (cons token (run-lexer token-readers (drop (token-length token) s)))))))

(defn lexer-fn
  "Create a lexer function based on a sequence of token readers. When executed, it
   will produce a lazy sequence of read tokens."
  [token-readers]
  (fn [in-seq & { :keys [token-count] }]
    (lazy-seq
      (when-let [rsq (run-lexer token-readers in-seq)]
        (cond token-count (take token-count rsq)
              :else rsq)))))

;; ---------------------------------------------------------------------
;; Lexer Macro
;; ---------------------------------------------------------------------
(defmacro lexer
  "Create a new Lexer using a series of token-type/matcher pairs.
   A matcher can be given as:
   - a string literal to match
   - a regular expression to match
   - a matcher function
   Example:

     (lexer
       :int #\"[1-9][0-9]+\"
       :+   \"+\"
       :-   \"-\"
       (:include ignore-whitespace))

   You can include existing lexers using the :include directive.
  "
  [& rules]
  `(let [token-readers# ~(create-token-readers rules)]
     (lexer-fn token-readers#)))
     
(defmacro deflexer
  "Define a new Lexer."
  [name & rules]
  `(def ~name (lexer ~@rules)))
