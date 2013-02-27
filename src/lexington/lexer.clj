(ns ^{ :doc "Lexington Lexer Facade"
       :author "Yannick Scherer" }
  lexington.lexer
  
  ;; Potemkin for Import
  (:use [potemkin :only [import-vars]])

  ;; Namespaces to import
  (:require [lexington.lexer.core :as c]
            [lexington.lexer.seq-matchers :as sqm]
            [lexington.lexer.utils :as u]))

(import-vars
  [lexington.lexer.utils
   
   discard
   retain
   with-string
   with-int
   generate
   generate-for
   generate-stateful
   allow-partial-input
   recover-from-error]

  [lexington.lexer.seq-matchers

   max-length
   min-length]

  [lexington.lexer.core

   lexer-fn
   lexer
   deflexer])
