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
     matches (might be zero).")
  (max-token-length [x]
    "Get maximum length of resulting tokens. This also restricts the number of elements
     passed to the function created by `matcher-for`."))

;; ## Matcher Generation

(defn- wrap-matcher
  "Wrap a function to fulfill the token matcher function contract (return a positive
   number or nil)."
  [f max-token-length]
  (fn [in-seq]
    (let [sq (if (and (integer? max-token-length) (pos? max-token-length))
               (take max-token-length in-seq)
               in-seq)]
      (when-let [r (f sq)]
        (when (and (integer? r) (pos? r))
          r)))))

(defn create-matcher
  "Create a matcher function based on the given value that produces a positive 
   number or nil."
  [v]
  (let [l (max-token-length v)]
    (when-let [f (matcher-for v)]
      (wrap-matcher f l))))

;; ## Predefined Matchers

(extend-type clojure.lang.AFunction
  Matchable
  (matcher-for [f] f)
  (max-token-length [_] 0))

(extend-type java.lang.String
  Matchable
  (matcher-for [s]
    (if-let [sq (seq s)]
      (let [c (count sq)]
        (fn [in-seq]
          (when-let [isq (seq in-seq)]
            (let [msq (take c isq)]
              (when (= msq sq)
                c)))))
      (constantly 0)))
  (max-token-length [s]
    (count s)))

(def ^:dynamic *max-regex-token-length*
  "Maximum Length of Tokens resulting from Regex Application."
  255)

(extend-type java.util.regex.Pattern
  Matchable
  (matcher-for [p]
    (let [re (re-pattern (str "^(" p ")"))]
      (fn [in-seq]
        (when-let [r (re-find re (apply str in-seq))]
          (if (coll? r)
            (count (first r))
            (count r))))))
  (max-token-length [_]
    *max-regex-token-length*))

;; ## Customize Matchers

;; ### Restrict Token Length of a given Matcher

(deftype MaxLengthRestrictionMatcher [m length]
  Matchable
  (matcher-for [_]
    (matcher-for m))
  (max-token-length [_] length))

(defn max-length
  "Restrict the length of a given Matcher."
  [matcher length]
  (MaxLengthRestrictionMatcher. matcher length))

;; ### Require a minimum Token Length

(deftype MinLengthRestrictionMatcher [m length]
  Matchable
  (matcher-for [_]
    (let [f (matcher-for m)]
      (fn [in-seq]
        (when-let [r (f in-seq)]
          (when-not (< r length)
            r)))))
  (max-token-length [_] 
    (max-token-length m)))

(defn min-length
  "Set the minimum length of a given Matcher."
  [matcher length]
  (MinLengthRestrictionMatcher. matcher length))
