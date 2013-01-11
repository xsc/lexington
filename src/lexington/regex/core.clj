(ns ^{ :doc "Lexington Regular Expressions"
       :author "Yannick Scherer" }
  lexington.regex.core
  (:require [lexington.fsm.core :as fsm]
            [lexington.fsm.fn :as fsm-fn] 
            [lexington.fsm.nfa :as nfa]
            [lexington.fsm.dfa :as dfa :only [invert-dfa]]
            [lexington.fsm.minimize :as m :only [minimize-dfa]]
            [lexington.fsm.consts :as c :only [any]]
            [clojure.string :as string :only [join]])
  (:use lexington.regex.util))

;; ## Regex Protocol

(defprotocol Regex
  "Protocol for Regular Expressions."
  (regex->nfa [this]
    "Produce NFA matching the Regular Expression.")
  (pattern [this]
    "Get java.util.regex.Pattern corresponding to this Regular Expression."))

;; ## Types of Regular Expressions

;; ### Literals/Choices

(deftype RegexLiteral [sq]
  Regex
  (regex->nfa [_]
    (let [c (count sq)]
      (cond (zero? c) nil
            (= c 1) (->
                      (fsm/dfa* [:q0 (first sq) :q1])
                      (fsm/accept-in :q1))
            :else (let [state-names (rx-state-name-seq (inc c))
                        transitions (map (fn [[from to] [in _]]
                                           (vector from in to))
                                         (partition 2 1 state-names)
                                         (partition 2 1 (concat sq [nil])))]
                    (->
                      (reduce 
                        (fn [dfa [from in to]]
                          (fsm/add-transition dfa from in to))
                        (fsm/dfa*)
                        transitions)
                      (fsm/initial-state (first state-names))
                      (fsm/accept-in (last state-names)))))))
  (pattern [_]
    (rx-escape sq)))

(defn rx-literal
  "Create new Regular Expression Literal."
  [sq]
  (RegexLiteral. sq))

(deftype RegexChoice [sq]
  Regex
  (regex->nfa [_]
    (->
      (reduce 
        (fn [dfa in]
          (fsm/add-transition dfa :q0 in :q1))
        (fsm/dfa*)
        (distinct sq))
      (fsm/accept-in :q1)))
  (pattern [_]
    (str "[" (rx-escape sq) "]")))

(defn rx-choice
  "Create new Regular Expression Choice."
  [sq]
  (RegexChoice. sq))

(deftype RegexInverseChoice [sq]
  Regex
  (regex->nfa [_]
    (->
      (reduce 
        (fn [dfa in]
          (fsm/add-transition dfa :q0 in c/reject!))
        (fsm/dfa*)
        (distinct sq))
      (fsm/add-transition :q0 c/any :q1)
      (fsm/accept-in :q1)))
  (pattern [_]
    (str "[^" (rx-escape sq) "]")))

(defn rx-not
  "Create new Regular Expression Inverse Choice."
  [sq]
  (RegexInverseChoice. sq))

;; ### Combinations

(deftype RegexSequence [rxs]
  Regex
  (regex->nfa [_]
    (apply nfa/concat-nfa (map regex->nfa rxs)))
  (pattern [_]
    (apply str (map pattern rxs))))

(defn rx-concat
  "Concatenate a number of Regular Expressions."
  [& rxs]
  (RegexSequence. rxs))

(deftype RegexUnion [rxs]
  Regex
  (regex->nfa [_]
    (apply nfa/union-nfa (map regex->nfa rxs)))
  (pattern [_]
    (str "(" (string/join "|" (map pattern rxs)) ")")))

(defn rx-union
  "Create Union of a number of Regular Expressions."
  [& rxs]
  (RegexUnion. rxs))

(deftype RegexPlus [rx]
  Regex
  (regex->nfa [_]
    (nfa/loop-nfa (regex->nfa rx)))
  (pattern [_]
    (str "(" (pattern rx) ")+")))

(defn rx-plus
  "Create new Regular Expression matching the given one at least once."
  [rx]
  (RegexPlus. rx))

(deftype RegexStar [rx]
  Regex
  (regex->nfa [_]
    (nfa/loop0-nfa (regex->nfa rx)))
  (pattern [_]
    (str "(" (pattern rx) ")*")))

(defn rx-star
  "Create new Regular Expression matching the given one any number of times."
  [rx]
  (RegexStar. rx))

(deftype RegexQuestionMark [rx]
  Regex
  (regex->nfa [_]
    (fsm/accept-empty (regex->nfa rx)))
  (pattern [_]
    (str "(" (pattern rx) ")?")))

(defn rx-question-mark
  "Create new Regular Expression matching the given one once or not at all."
  [rx]
  (RegexQuestionMark. rx))

;; ### Shorthands

(extend-type java.lang.String
  Regex
  (regex->nfa [this]
    (regex->nfa 
      (rx-literal (seq this))))
  (pattern [this]
    this))

(extend-type java.lang.Object
  Regex
  (regex->nfa [this]
    (regex->nfa
      (rx-literal [this])))
  (pattern [this]
    (str this)))

;; ### Prepared Regular Expression

(deftype RegexNFA [nfa p]
  Regex
  (regex->nfa [_]
    nfa)
  (pattern [_]
    p))

(defn rx-prepare
  "Create the underlying NFA for a Regular Expression, wrapped in a new
   Regular Expression. This should be done when reusing Regexes to avoid
   unnecessary rebuilding of the NFA."
  [rx]
  (RegexNFA. 
    (regex->nfa rx)
    (pattern rx)))

;; ### Minimize Regular Expression

(defn rx-minimize
  "Minimize the Regular Expression's underlying NFA."
  [rx]
  (RegexNFA.
    (m/minimize-dfa (regex->nfa rx) :algorithm :brz-unnormalized)
    (pattern rx)))

;; ## Executing Regular Expressions

(defn rx-matcher
  "Create matcher function from regular expression. An option parameter, either `:greedy` 
   or `:non-greedy`, can be supplied to specify the kind of matcher. The resulting function
   can directly be applied to (possibly lazy) input sequences, returning the length of a
   matching prefix."
  ([nfa] (rx-matcher nfa nil))
  ([rx match]
   (fsm-fn/prefix-match-count-fn 
     (nfa/epsilon-nfa->nfa (regex->nfa rx))
     :match (if (or (= match :greedy) (= match :non-greedy))
              match
              :greedy))))

(defn rx-matcher-all
  "Create lazy sequence of [position length] pairs detailing all matches for the given
   matcher function in the given input seq."
  [matcher sq]
  (letfn [(lazy-find [sq pos]
            (lazy-seq
              (loop [sq sq
                     pos pos]
                (when (seq sq)
                  (if-let [r (matcher sq)]
                    (if (zero? r)
                      (cons [pos 0] (lazy-find (rest sq) (inc pos)))
                      (cons [pos r] (lazy-find (drop r sq) (+ r pos))))
                    (recur (rest sq) (inc pos)))))))]
    (lazy-find sq 0)))

(defn rx-matcher-find-all
  "Create lazy sequence of results obtained by repeatedly applying the given matcher to
   different positions in the given seq. `sq` should be finite but might be lazy."
  [matcher sq]
  (letfn [(lazy-find [sq]
            (lazy-seq
              (loop [sq sq]
                (when (seq sq)
                  (if-let [r (matcher sq)]
                    (if (zero? r)
                      (cons [] (lazy-find (rest sq)))
                      (cons (take r sq) (lazy-find (drop r sq))))
                    (recur (rest sq)))))))]
    (lazy-find sq)))
 
(defn rx-matcher-find-first
  "Find first match for the given matcher in the given sequence."
  [matcher sq]
  (first (rx-matcher-find-all matcher sq)))

;; ## Regular Expression DSL
;;
;; Example: Matching String Literals "..." (with escape sequences)
;;
;;    (def str-rx (rx \" ([not \\ \"] | \\ [\\ \" \r \n \t]) * \"))
;;    (def match-str (rx-matcher str-rx))
;; 
;;    (prn (match-str "\"\""))           ;= 2
;;    (prn (match-str "\"abc\""))        ;= 5
;;    (prn (match-str "\"abc\\\"def\"")) ;= 10
;;    (prn (match-str "\"abc"))          ;= nil
;;    (prn (match-str "\"abc\" def\""))  ;= 5

(defmulti rx-expand 
  "Expand list of symbols/vectors/lists/characters into Regular Expression creation call:

      \\x        -> (rx-literal [\\x])
      [\\x]      -> (rx-choice [\\x])
      [not \\x]  -> (rx-not [\\x])
      ... *      -> (rx-star ...)
      ... +      -> (rx-plus ...)
      ... ?      -> (rx-question-mark ...)
      ... | ...  -> (rx-union (rx-expand ...) (rx-expand ...))
  "
  class
  :default nil)

(defmethod rx-expand nil
  [x]
  x)

(defmethod rx-expand clojure.lang.IPersistentVector
  [v]
  (let [[f & rst] v]
    (if (symbol= f "not")
      `(rx-not [~@rst])
      `(rx-choice [~@v]))))

(defmethod rx-expand clojure.lang.ISeq
  [sq]
  (let [parts (split-with (complement #(symbol= % "|")) sq)
        [a b] parts
        a (seq a) b (seq (rest b))]
    (cond 
      (and a b)
        `(rx-union
           ~(rx-expand (first parts))
           ~(rx-expand (rest (second parts))))
      (and (not a) b) 
        (rx-expand b)
      (and (not a) (not b)) 
         nil
      :else `(rx-concat
               ~@(reduce
                   (fn [r x]
                     (if (symbol? x)
                       (condp symbol= x
                         "*" (conj (pop r) `(rx-star ~(last r)))
                         "+" (conj (pop r) `(rx-plus ~(last r)))
                         "?" (conj (pop r) `(rx-question-mark ~(last r)))
                         (conj r (rx-expand x)))
                       (conj r (rx-expand x))))
                   [] sq)))))

(defmacro rx 
  "Create new Regular Expression object. The following constructs are supported:

   - `... *` : match the previous expression any number of times (even not at all);
   - `... +` : match the previous expression at least once;
   - `... ?` : match the previous expression once or not at all;
   - `... | ...` : match either one of the given sides;
   - `[...]` : match any of the given input entities;
   - `[not ...] : match none of the given input entities;
   - `(...)` : group expressions together.

   Example (a matcher for string literals):

     (rx \\\" ([not \\\\ \\\"] | \\\\ [\\\\ \\\" \\r \\n \\t]) * \\\")

   This is equivalent to the pattern:

     #\"\\\"([^\\\\\\\"]|\\\\[\\\\\\\"rnt])*\\\"\"
  "
  [& args]
  (rx-expand args))
