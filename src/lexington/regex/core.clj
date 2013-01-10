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

;; ## Regex Type

(defrecord Regex [pattern nfa])

;; ## Building Regular Expressions

;; ### Builders

(defn rx-literal
  "Create DFA from seq, using each element as an transition input. The resulting
   FSM will accept exactly the given sequence in exactly the given order."
  [sq]
  (let [c (count sq)]
    (cond (zero? c) nil
          (= c 1) (Regex.
                    (str (first sq))
                    (->
                      (fsm/dfa* [:q0 (first sq) :q1])
                      (fsm/accept-in :q1)))
          :else (let [state-names (rx-state-name-seq (inc c))
                      transitions (map (fn [[from to] [in _]]
                                         (vector from in to))
                                       (partition 2 1 state-names)
                                       (partition 2 1 (concat sq [nil])))
                      dfa (->
                            (reduce 
                              (fn [dfa [from in to]]
                                (fsm/add-transition dfa from in to))
                              (fsm/dfa*)
                              transitions)
                            (fsm/initial-state (first state-names))
                            (fsm/accept-in (last state-names)))]
                  (Regex. (rx-escape sq) dfa)))))

(defn rx-choice
  "Create DFA that accepts a single input element: any element of the given seq."
  [sq]
  (Regex.
    (str "[" (rx-escape sq) "]")
    (->
      (reduce 
        (fn [dfa in]
          (fsm/add-transition dfa :q0 in :q1))
        (fsm/dfa*)
        (distinct sq))
      (fsm/accept-in :q1))))

(defn rx-not
  "Create Regular Expression that accepts a single input element: any element that is not in the
   given seq."
  [sq]
  (Regex.
    (str "[^" (rx-escape (apply str sq)) "]")
    (->
      (reduce 
        (fn [dfa in]
          (fsm/add-transition dfa :q0 in c/reject!))
        (fsm/dfa*)
        (distinct sq))
      (fsm/add-transition :q0 c/any :q1)
      (fsm/accept-in :q1))))

(defn rx-concat
  "Concatenate a number of regular expressions."
  [& rxs]
  (let [n (apply nfa/concat-nfa (map :nfa rxs))
        p (apply str (map :pattern rxs))]
    (Regex. p n)))

(defn rx-union
  "Create union of a number of regular expressions."
  [& rxs]
  (let [n (apply nfa/union-nfa (map :nfa rxs))
        p (str "(" (string/join "|" (map :pattern rxs)) ")")]
    (Regex. p n)))

(defn rx-plus
  "Create regular expression that will accept the given one at least once."
  [{:keys[pattern nfa]}]
  (Regex. (str "(" pattern ")+") 
          (nfa/loop-nfa nfa)))

(defn rx-star
  "Create regular expression that will accept the given one any number of times."
  [{:keys[pattern nfa]}]
  (Regex. (str "(" pattern ")*") 
          (nfa/loop0-nfa nfa)))

(defn rx-question-mark
  "Create regular expression that will accept the given one at most once."
  [{:keys[pattern nfa]}]
  (Regex. (str "(" pattern ")?") 
          (fsm/accept-empty nfa)))

(defn rx-minimize
  "Minimize the Regular Expression's underlying NFA."
  [{:keys[pattern nfa]}]
  (Regex. pattern 
          (m/minimize-dfa nfa 
                          :algorithm :brz-unnormalized)))

;; ## Executing Regular Expressions

(defn rx-matcher
  "Create matcher function from regular expression. An option parameter, either `:greedy` 
   or `:non-greedy`, can be supplied to specify the kind of matcher. The resulting function
   can directly be applied to (possibly lazy) input sequences, returning the length of a
   matching prefix."
  ([nfa] (rx-matcher nfa nil))
  ([{:keys[nfa]} match]
   (fsm-fn/prefix-match-count-fn 
     (nfa/epsilon-nfa->nfa nfa)
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

(defn rx-expand
  "Expand list of symbols/vectors/lists/characters into Regular Expression creation call:

      \\x        -> (rx-literal [\\x])
      [\\x]      -> (rx-choice [\\x])
      [not \\x]  -> (rx-not [\\x])
      ... *     -> (rx-star ...)
      ... +     -> (rx-plus ...)
      ... ?     -> (rx-question-mark ...)
      ... | ... -> (rx-union (rx-expand ...) (rx-expand ...))
  "
  [sq]
  (let [rx-rest (reduce
                  (fn [r x]
                    (cond (vector? x) (conj r
                                            (let [[f & rst] x]
                                              (if (symbol= f "not")
                                                `(rx-not [~@rst])
                                                `(rx-choice [~@x]))))
                          (symbol? x) (condp symbol= x
                                        "*" (conj (pop r) `(rx-star ~(last r)))
                                        "+" (conj (pop r) `(rx-plus ~(last r)))
                                        "?" (conj (pop r) `(rx-question-mark ~(last r))) 
                                        (conj r `(rx-literal [~x])))
                          (coll? x) (conj r (rx-expand x))
                          :else (conj r `(rx-literal [~x]))))
                  [] sq)]
    (if (= (count rx-rest) 1) 
      (first rx-rest)
      `(rx-concat ~@rx-rest))))

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
  (let [parts (split-with (complement #(symbol= % "|")) args)
        [a b] parts]
    (cond (and (seq a) (seq b)) `(rx-union
                                   (rx ~@(first parts))
                                   (rx ~@(rest (second parts))))
          (and (not (seq a)) (seq b)) `(rx ~@(rest b))
          (and (not (seq a)) (not (seq b))) nil
          :else (rx-expand a))))
