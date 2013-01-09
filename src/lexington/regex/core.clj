(ns ^{ :doc "Lexington Regular Expressions"
       :author "Yannick Scherer" }
  lexington.regex.core
  (:require [lexington.fsm.core :as fsm]
            [lexington.fsm.fn :as fsm-fn] 
            [lexington.fsm.nfa :as nfa]
            [lexington.fsm.minimize :as m]
            [clojure.string :as string :only [join]]))

;; ## Regex Type

(defrecord Regex [pattern nfa])

;; ## Building Regular Expressions

(defn- state-name-seq
  "Create lazy seq of state names `:q0`, `:q1`, ... using the given range
   (specified exactly as if using `range`)."
  [& range-args]
  (map
    (fn [i]
      (keyword (str "q" i)))
    (apply range range-args)))

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
          :else (let [state-names (state-name-seq (inc c))
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
                  (Regex. (apply str sq) dfa)))))

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
