(ns ^{ :doc "Lexington FSM Representation"
       :author "Yannick Scherer" }
  lexington.fsm.core
  (:require [lexington.fsm.states :as s]
            [lexington.fsm.transitions :as t]
            [lexington.fsm.errors :as e]
            [lexington.fsm.fsm :as fsm]))

;; ## Preface
;;
;; It shall only be necessary to include this namespace into a project using FSMs. Thus,
;; we create wrappers around certain functions from other namespaces, as well as macros
;; that automatically resolve references to said functions.

;; ## Wrappers

(defn state
  "Create new state."
  [k & transitions]
  (s/new-state k transitions))

(defn accept-state
  "Create new accepting state."
  [k & transitions]
  (s/new-accept-state k transitions))

(defn reject-state
  "Create new rejecting state."
  [k]
  (s/new-reject-state k))

(defn new-fsm
  "Create new FSM from a list of states."
  [& states]
  (fsm/states->fsm states))

;; ## fsm*
;;
;; The `fsm*` macro introduces shorthands for the different special transitions and target states:
;; 
;; - `*` represents the `(any)` transition
;; - `(:! ...)` represents the `(except ...)` transition
;; - `(:? ...)` represents the `(one-of ...)` transition
;; - `accept!` represents the `(accept)` destination state
;; - `reject!` represents the `(reject)` destination state
;; - `_` represents the `(continue)` destination state
;;
;; Additionally, it handles the creation of accepting/rejecting/normal states by using special 
;; keywords, and tries to increase readability by using "->" between input entity and destination
;; state:
;;
;;     (fsm* 
;;       (:state :a
;;         \a -> _
;;         \b -> accept!
;;         \c -> :c
;;         *  -> :r)
;;       (:accepting :c
;;         \c -> :a)
;;       (:rejecting :r))
;;
;; This will expand to:
;;
;;     (states->fsm [ 
;;       (new-state :a [ \a (continue) 
;;                       \b (accept) 
;;                       \c :c 
;;                       (any) :r ])
;;       (new-accept-state :c [ \c :a ]) 
;;       (new-reject-state :r) ])
;;
;; The FSM will accept languages matching the regular expression "a*(cca*)*b", of whatever use
;; that might be.

(defmacro fsm*
  "Generate FSM from a list of states with special keywords and symbols, e.g.:

    (fsm* 
      (:state :a
        (:! 'a 'b 'd) -> :c
        'a            -> _
        'b            -> accept!
        *             -> :r)
      (:accepting :c
        'c -> :a)
      (:rejecting :r))
  "
  [& states]
  (letfn [(get-name [k]
            (and (or (keyword? k)
                     (symbol? k))
                 (name k)))
          (convert-transitions [n transitions]
            (vec 
              (mapcat 
                (fn [[input arrow next-state]]
                  (when (not (= (get-name arrow) "->"))
                    (e/error "Missing '->' in transition [" input " -> ...] of state " n))
                  (vector (if (coll? input)
                            (let [[d & args] input
                                  sym (get-name d)]
                              (cond (= sym "!") `(t/except ~@args)
                                    (= sym "?") `(t/one-of ~@args)
                                    :else input))
                            (let [sym (get-name input)]
                              (cond (= sym "*") `(t/any)
                                    :else input)))
                          (let [sym (get-name next-state)]
                            (cond (= sym "_") `(s/continue)
                                  (= sym "accept!") `(s/accept)
                                  (= sym "reject!") `(s/reject)
                                  :else next-state))))
                (partition 3 transitions))))]
    (let [states* (map (fn [s]
                         (if-not (coll? s) s
                           (let [[k & [n & t]] s
                                  k (get-name k)]
                             (cond (= k "state")  `(s/new-state ~n ~(convert-transitions n t))
                                   (= k "accepting") `(s/new-accept-state ~n ~(convert-transitions n t))
                                   (= k "rejecting") `(s/new-reject-state ~n)
                                   :else s))))
                       states)]
      `(fsm/states->fsm ~(vec states*)))))

;; ## FSM Function Generation

(defn- next-state
  "Get the next state of an FSM for a given input."
  [transitions current-state input]
  (let [st (transitions current-state)]
    (or (st input)
        (st (t/any)))))

;; ### fsm->check-fn
;;
;; Each FSM can be transformed into a function that takes an input sequence and produces either nil (if
;; the input sequence was rejected) or the final state if it was accepted.

(defn fsm->check-fn
  "Generate function from an FSM that returns either `nil` (if the FSM does not recognize an input sequence)
   or the final accepting state."
  [{:keys[initial transitions accept reject]}]
  (fn [input]
    (loop [state initial
           input (seq input)]
      (if-not (seq input)
        (accept state)
        (let [n (next-state transitions state (first input))]
          (when-not (reject n)
            (recur n (rest input))))))))

;; ### fsm->count-fn
;;
;; Each FSM can be transformed into a counter function that returns the number of input entities it read
;; until it ended up in an accepting state. Recognition can either be done greedy (as much as possible,
;; return highest acceptor count) or non-greedy (default; return once you are in an accepting state)

(defn fsm->count-fn
  "Generate a function from an FSM that returns either `nil` (if the FSM never enters an accepting state) or
   the number of input entities processed until an accepting state was reached. The two-argument variant
   of this functions takes a counting strategy as its first parameter, either `:greedy` (return the number of 
   the last entered accepting state) or `:non-greedy`(return the number of the first entered accepting state)."
  ([fsm] (fsm->count-fn :greedy fsm))
  ([k {:keys[initial transitions accept reject]}]
   (let [greedy? (= k :greedy)]
     (if (and (not greedy?) (accept initial))
       (constantly 0)
       (fn [input]
         (loop [state       initial
                input       (seq input)
                counter     0
                last-accept (and (accept initial) 0)]
           (if-not (seq input)
             (if (accept state)
               counter
               last-accept)
             (let [n (next-state transitions state (first input))
                   counter (inc counter)]
               (cond (reject n) last-accept
                     (and (not greedy?) (accept n)) counter
                     (accept n) (recur n (rest input) counter counter)
                     :else (recur n (rest input) counter last-accept))))))))))

;; ### fsm->trace-fn
;;
;; Each FSM can be transformed into a function that returns a lazy sequence of the states produced by an input sequence,
;; where the last element is either the first rejecting state or the one the FSM was in when the sequence ended.

(defn fsm->trace-fn
  "Generate a function from an FSM that returns a lazy sequence with the names of the states the FSM entered by 
   processing the sequence, either up to the first rejecting state or the state at the end of the sequence."
  [{:keys[initial transitions reject]}]
  (letfn [(trace-lazy [current-state input]
            (lazy-seq
              (when (and (not (reject current-state)) (seq input))
                (let [n (next-state transitions current-state (first input))]
                  (cons n (trace-lazy n (rest input)))))))]
    (fn [input]
      (cons initial (trace-lazy initial input)))))

