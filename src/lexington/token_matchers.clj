(ns ^{ :doc "Token Matcher Functions."
       :author "Yannick Scherer" }
  lexington.token-matchers)

;; ## Token Matcher Concept

(defmulti matcher-for
  "Multimethod to create a matcher function based on a given literal. A matcher
   function returns the number of input entities matching it; nil otherwise."
  (fn [x] (class x)))

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

;; ## Token Matchers

(defmethod matcher-for java.lang.String
  [s]
  (when-let [sq (seq s)]
    (let [c (count sq)]
      (fn [in-seq]
        (when-let [isq (seq in-seq)]
          (let [msq (take c isq)]
            (when (= msq sq)
              c)))))))

(defmethod matcher-for java.util.regex.Pattern
  [p]
  (let [re (re-pattern (str "^(" p ")"))]
    (fn [in-seq]
      (when-let [r (re-find re (apply str in-seq))]
        (if (coll? r)
          (count (first r))
          (count r))))))

(defmethod matcher-for clojure.lang.IFn
  [f]
  f)

(defmethod matcher-for java.lang.Object
  [o]
  (fn [in-seq]
    (when-let [sq (seq in-seq)]
      (when (= (first sq) o)
        1))))

