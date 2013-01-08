(ns ^{ :doc "Token Reader creation functions for `lexer' macro."
       :author "Yannick Scherer" }
  lexington.lexer.token-readers
  (:use lexington.tokens
        lexington.lexer.seq-matchers))

;; ---------------------------------------------------------------
;; Handlers for Directives
;; ---------------------------------------------------------------
(defmulti directive->token-readers
  "Multimethod for Handling Lexer Directives like :include. Should produce
   a sequence of syntax-quoted functions."
  (fn [directive & _]
    directive))

(defmethod directive->token-readers :include
  [_ & lexers-to-include]
  (map (fn [lexer-function]
         `(fn [s#]
            (first (~lexer-function s#))))
       lexers-to-include))

(defmethod directive->token-readers :token
  [_ k m & _]
  (vector
    `(let [m# (create-matcher ~m)]
       (fn [sq#]
         (when-let [r# (m# sq#)]
           (new-token ~k (take r# sq#)))))))

;; ---------------------------------------------------------------
;; Code Generation Function
;; ---------------------------------------------------------------
(defn- normalize-rules
  "Converts a lexer rule sequence into a normalized representation, where each
   sequence element is a (<directive> ...) list."
  [rules]
  (loop [rules rules
         result []]
    (if-not (seq rules)
      result
      (let [[f & rst] rules]
        (if (coll? f)
          (recur rst
                 (conj result f))
          (recur (rest rst) 
                 (conj result (list* :token (take 2 rules)))))))))

(defn create-token-readers
  "Create sequence of token reader functions."
  [rules]
  (let [normalized (normalize-rules rules)]
    (vec (mapcat #(apply directive->token-readers %) normalized))))
