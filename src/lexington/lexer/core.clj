(ns ^{ :doc "Lexington Lexer Implementation."
       :author "Yannick Scherer" }
  lexington.lexer.core

  ;; Potemkin for Import
  (:use [potemkin :only [import-vars]])

  ;; Namespaces to import
  (:require [lexington.tokens :as tk]
            [lexington.lexer.token-readers :as tr]
            [lexington.lexer.seq-matchers :as sqm]
            [lexington.lexer.utils :as u]))

;; ## Import

(import-vars
  [lexington.lexer.utils
   
   discard
   retain
   with-string
   with-int
   generate
   generate-for
   generate-stateful])

;; ## Lexers
;;
;; A lexer is a function that takes a sequence of input entities (e.g. characters) and produces a 
;; sequence of tokens. 
;;
;; ## Lexer Helpers
;;
;; Since the `lexer` macro basically just creates a series of trial-and-error token reader
;; functions, these helpers handle the actual application of those functions.

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
        (cons token (run-lexer token-readers (drop (tk/token-length token) s)))))))

(defn lexer-fn
  "Create a lexer function based on a sequence of token readers. When executed, it
   will produce a lazy sequence of read tokens."
  [token-readers]
  (fn [in-seq & {:keys[token-count]}]
      (when-let [rsq (run-lexer token-readers in-seq)]
        (cond token-count (take token-count rsq)
              :else rsq))))

;; ## Lexer Macro
;;
;; Lexers can be created using the `lexer` and `deflexer` macros which associate token types with 
;; a description of how and when to produce the corresponding token value. There a several possible formats
;; for these _matching instructions_:
;;
;; - a string literal to expect at the beginning of the input
;; - a regular expression to match against the start of the input
;; - a function returning the number of input sequence elements it matched (or nil)
;; - etc ...
;;
;; All these things will be passed to `lexington.token-matchers/create-matcher` to produce the matching function 
;; to be used.
;; 
;;     (deflexer simple-math
;;       :integer #"[1-9][0-9]*"
;;       :plus    "+"
;;       :minus   #(when (= (first %) \-) 1))
;;
;; It is possible to use certain directives inside of a lexer, e.g. `:include`:
;;
;;     (deflexer more-math
;;       (:include simple-math)
;;       :mul "*"
;;       :div "/")
;;
;; Custom Directives can be introduced by implementing the multimethod `lexington.token-readers/directive->token-reader`.

(defmacro lexer
  "Create a new Lexer using a series of token-type/matcher pairs."
  [& rules]
  `(let [token-readers# ~(tr/create-token-readers rules)]
     (lexer-fn token-readers#)))
     
(defmacro deflexer
  "Define a new Lexer."
  [name & rules]
  `(def ~name (lexer ~@rules)))
