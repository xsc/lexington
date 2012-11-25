(ns ^{ :doc "Lexington FSM to function converters."
       :author "Yannick Scherer" }
  lexington.fsm.fn
  (:require [lexington.fsm.transitions :only [any] :as t]))

;; ## Helpers

(def next-state
  "Get the next state of an FSM for a given input."
  (let [any-transition (t/any)]
    (fn [transitions current-state input]
      (let [st (get transitions current-state)]
        (or (get st input)
            (get st any-transition))))))

;; ## Base Converters
;;
;; ### fsm->reduce-fn
;;
;; Each FSM can be transformed into a function that takes an input sequence and an optional initial 
;; value, then traverses the FSM, adjusting the given value on each step. It returns either nil (if
;; the FSM ended up in a rejecting state), or the final value.

(defn fsm->reduce-fn
  "Generate function from FSM that calls the given function on each transition, providing the
   current value, the source and destination states, as well as the processed entity. Returns
   the final value if the FSM ends in an accepting state or `nil` if it encounters any rejecting
   one."
  [f initial-value {:keys[initial transitions accept reject]}]
  (if (reject initial)
    (constantly nil)
    (fn [input]
      (loop [state initial
             input (seq input)
             v     initial-value]
        (if-not (seq input)
          (when (accept state) v)
          (let [n (next-state transitions state (first input))]
            (when-not (reject n)
              (recur n (rest input) (f v state n (first input))))))))))

;; ### fsm->trace-reduce-fn
;;
;; Behaves like `fsm->reduce-fn` but returns a lazy sequence of the different function results returned on each
;; transition.

(defn fsm->trace-reduce-fn
  "Generate function from FSM that calls the given function on each transition, providing the
   current value, the source and destination states, as well as the processed entity. Returns
   a lazy sequence of the function results returned at each transition."
  [f initial-value {:keys[initial transitions accept reject]}]
  (letfn [(trace-lazy [current-state current-value input]
            (lazy-seq
              (when (and (not (reject current-state)) (seq input))
                (let [n (next-state transitions current-state (first input))
                      v (f current-value current-state n (first input))]
                  (cons v (trace-lazy n v (rest input)))))))]
    (if (reject initial)
      (constantly nil)
      (fn [input]
        (cons initial-value (trace-lazy initial initial-value input))))))

;; ### fsm->find-reduce-fn
;;
;; Behaves like `fsm->reduce-fn`but returns a lazy sequence of the different function results returned on each 
;; transition to an accepting state.

(defn fsm->find-reduce-fn
  "Generate function from FSM that calls the given function on each transition, providing the
   current value, the source and destination states, as well as the processed entity. Returns
   a lazy sequence of the function results returned at each transition into an accepting state."
  [f initial-value {:keys[initial transitions accept reject]}]
  (letfn [(find-lazy [current-state current-value input]
            (lazy-seq
              (when (and (not (reject current-state)) (seq input))
                (let [n (next-state transitions current-state (first input))
                      v (f current-value current-state n (first input))]
                  (if (accept n)
                    (cons v (find-lazy n v (rest input)))
                    (find-lazy n v (rest input)))))))]
    (if (reject initial)
      (constantly nil)
      (fn [input]
        (let [found (find-lazy initial initial-value input)]
          (if (accept initial)
            (cons initial-value found)
            found))))))

;; ## Derived Converters
;;
;; ### fsm->check-fn
;;
;; Each FSM can be transformed into a function that takes an input sequence and produces either nil (if
;; the input sequence was rejected) or the final state if it was accepted.

(defn fsm->check-fn
  "Generate function from an FSM that returns either `nil` (if the FSM does not recognize an input sequence)
   or the final accepting state."
  [{:keys[initial] :as fsm}]
  (fsm->reduce-fn 
    (fn [_ _ dest-state _]
      dest-state)
    initial
    fsm))

;; ### fsm->trace-fn

(defn fsm->trace-fn
  "Create function from FSM that returns a lazy sequence of the states the FSM traverses for a 
   specific input."
  [{:keys[initial] :as fsm}]
  (fsm->trace-reduce-fn
    (fn [_ _ dest-state _]
      dest-state)
    initial
    fsm))


;; ### fsm->count-fn
;;
;; Each FSM can be transformed into a counter function that returns the number of input entities it read
;; until it ended up in an accepting state.,

(defn- fsm->greedy-count-fn
  "Generate counter function that tries to accept the maximum number of input entities."
  [fsm]
  (comp last
        (fsm->find-reduce-fn
          (fn [counter & _]
            (inc counter))
          0
          fsm)))

(defn- fsm->non-greedy-count-fn
  "Generate counter function that tries to accept the minimum number of input entities."
  [fsm]
  (comp first
        (fsm->find-reduce-fn
          (fn [counter & _]
            (inc counter))
          0
          fsm)))

(defn- fsm->total-count-fn
  "Generate counter function that counts the times the FSM enters an accepting state."
  [fsm]
  (comp count (fsm->find-reduce-fn (constantly nil) nil fsm)))

(defn fsm->count-fn
  "Generate a function from an FSM that returns either `nil` (if the FSM never enters an accepting state) or
   the number of input entities processed until an accepting state was reached. The two-argument variant
   of this functions takes a counting strategy as its first parameter, either `:greedy` (return the number of 
   the last entered accepting state; this is the default), `:non-greedy` (return the number of the first 
   entered accepting state) or `:total` (return the number of accepting states entered)."
  ([fsm] (fsm->count-fn :greedy fsm))
  ([k fsm]
   (cond (= k :greedy) (fsm->greedy-count-fn fsm)
         (= k :non-greedy) (fsm->non-greedy-count-fn fsm)
         (= k :total) (fsm->total-count-fn fsm)
         :else (constantly nil))))
