(ns ^{ :doc "Token Matcher Functions."
       :author "Yannick Scherer" }
  lexington.token-matchers)

(defmulti create-matcher
  "Multimethod to create a matcher function based on a given literal. A matcher
   function returns the number of input entities matching it; nil otherwise."
  (fn [x] (class x)))

(defmethod create-matcher java.lang.String
  [s]
  (when-let [sq (seq s)]
    (let [c (count sq)]
      (fn [in-seq]
        (when-let [isq (seq in-seq)]
          (let [msq (take c isq)]
            (when (= msq sq)
              c)))))))

(defmethod create-matcher java.util.regex.Pattern
  [p]
  (let [re (re-pattern (str "^" p))]
    (fn [in-seq]
      (when-let [r (re-find re (apply str in-seq))]
        (count r)))))

(defmethod create-matcher clojure.lang.IFn
  [f]
  f)

(defmethod create-matcher java.lang.Object
  [o]
  (fn [in-seq]
    (when-let [sq (seq in-seq)]
      (when (= (first sq) o)
        1))))

