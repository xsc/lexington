(ns ^{ :doc "Regular Expression Parsing from String"
       :author "Yannick Scherer" }
  lexington.regex.parser
  (:use [lexington.regex.core :only [rx-expand]]
        lexington.regex.util))

;; ## Parrens

(def ^:private parrens { \( \) \[ \] \{ \} })
(def ^:private parren-open? (set (keys parrens)))
(def ^:private parren-close? (set (vals parrens)))

(defn rx-parren-match?
  "Do two parrens match?"
  [a b]
  (cond (parren-open? a) (= (parrens a) b)
        (parren-open? b) (= (parrens b) a)
        :else nil))

;; ## Regex Parsing (from String/Pattern)

(defn parren-level-seq
  "Create seq of 'levels' for each character. An opening parren increments the level,
   while a closing parren decrements it."
  [sq]
  (letfn [(lazy-level [sq n parren-stack escape]
            (lazy-seq
              (if (seq sq)
                (let [[x & rst] sq]
                  (cond escape (if (rx-escape-char? x)
                                 (cons n (lazy-level rst n parren-stack nil))
                                 (throw (Exception. (str "Illegal Escape Sequence: \\" x))))
                        (= x \\) (cons n (lazy-level rst n parren-stack true))
                        (parren-open? x) (cons (inc n) (lazy-level rst (inc n) (conj parren-stack x) nil))
                        (parren-close? x) (let [p (peek parren-stack)]
                                           (if (rx-parren-match? p x)
                                             (cons n (lazy-level rst (dec n) (pop parren-stack) nil))
                                             (throw (Exception. (str "Unmatched Parren: " p)))))
                        :else (cons n (lazy-level rst n parren-stack nil))))
                (when (seq parren-stack)
                  (throw (Exception. (str "Unmatched Parrens: " parren-stack)))))))]
    (lazy-level sq 0 [] nil)))

(defn- split-parts
  "Split a seq of characters into parts that have to be specially treated by `rx-seq`. This mainly
   creates seqs of parren-enclosed constructs and those that are not, e.g.:
  
     (split-parts (seq \"abc(def)*\"))
     ;; => ((\\a \\b \\c) (\\( \\d \\e \\f \\)) (\\*))
  "
  [sq]
  (let [sq (map vector sq (parren-level-seq sq))]
    (letfn [(lazy-split [sq p? not-p?]
              (lazy-seq
                (when (seq sq)
                  (let [[p rst] (split-with p? sq)]
                    (if (seq p)
                      (cons (map first p) (lazy-split rst not-p? p?))
                      (lazy-split rst not-p? p?))))))]
      (lazy-split sq #(= (second %) 0) #(> (second %) 0)))))

(defn- rx-seq
  "Create structured parameter list for `rx` from seq of characters."
  [sq]
  (let [parts (split-parts sq)]
    (->> parts
      (mapcat
        (fn [expr]
          (when (seq expr)
            (let [expr (map (fn [x]
                              (case x
                                \* '*
                                \+ '+
                                \? '?
                                \| '|
                                x)) expr)]
              (case (first expr)
                \( [(rx-seq (rest (butlast expr)))]
                \[ (vector
                     (vec 
                       (if (= (second expr) \^)
                         (cons 'not (rx-seq (butlast (rest (rest expr)))))
                         (rx-seq (rest (butlast expr))))))
                expr))))))))

(defmacro rx-str
  "Create Regular Expression from String or java.util.regex.Pattern. Every construct
   supported by `rx` is accepted by this macro."
  [s]
  (let [s (cond (= (class s) java.util.regex.Pattern)  (rx-unescape (str s))
                (string? s) s
                :else (throw (Exception. (str "expects string or pattern as parameter; given: " s))))]
    `(-> ~(rx-expand (rx-seq (seq s)))
       (assoc :pattern ~s))))
