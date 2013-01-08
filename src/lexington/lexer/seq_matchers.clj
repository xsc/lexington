(ns ^{ :doc "Seq Matcher Utilities."
       :author "Yannick Scherer" }
  lexington.lexer.seq-matchers)

;; ## Protocol `Matchable`

(defprotocol Matchable
  "Protocol implemented by entities that can be used to generate seq matcher 
   functions"
  (matcher-for [x]
    "Create function that matches a seq against the given entity. A matcher function 
     returns either nil (if no match was found) or the number of seq elements it
     matches (might be zero)."))

;; ## Matcher Generation

(defn- wrap-matcher
  "Wrap a function to fulfill the token matcher function contract (return a positive
   number or nil)."
  [f]
  (fn [in-seq]
    (when-let [r (f in-seq)]
      (when (and (integer? r) (pos? r))
        r))))

(defn create-matcher
  "Create a matcher function based on the given value that produces a positive 
   number or nil."
  [v]
  (when-let [f (matcher-for v)]
    (wrap-matcher f)))

;; ## Predefined Matchers

(extend-protocol Matchable
 
  clojure.lang.IFn
  (matcher-for [f]
    (wrap-matcher f))

  java.lang.String
  (matcher-for [s]
    (if-let [sq (seq s)]
      (let [c (count sq)]
        (fn [in-seq]
          (when-let [isq (seq in-seq)]
            (let [msq (take c isq)]
              (when (= msq sq)
                c)))))
      (constantly 0)))

  java.util.regex.Pattern
  (matcher-for [p]
    (let [re (re-pattern (str "^(" p ")"))]
      (fn [in-seq]
        (when-let [r (re-find re (apply str in-seq))]
          (if (coll? r)
            (count (first r))
            (count r))))))

  )
