(ns ^{ :doc "Lexington FSM to function converters."
       :author "Yannick Scherer" }
  lexington.fsm.fn
  (:require [lexington.fsm.transitions :only [any] :as t]))

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
;; until it ended up in an accepting state.,

(defn- fsm->greedy-count-fn
  "Generate counter function that tries to accept the maximum number of input entities."
  [{:keys[initial transitions accept reject]}]
  (if (reject initial) 
    (constantly nil)
    (fn [input]
      (loop [state initial
             input (seq input)
             counter 0
             last-accept (and (accept initial) 0)]
        (if-not (seq input)
          (if (accept state)
            counter
            last-accept)
          (let [n (next-state transitions state (first input))
                counter (inc counter)]
            (cond (reject n) last-accept
                  (accept n) (recur n (rest input) counter counter)
                  :else (recur n (rest input) counter last-accept))))))))

(defn- fsm->non-greedy-count-fn
  "Generate counter function that tries to accept the minimum number of input entities."
  [{:keys[initial transitions accept reject]}]
  (cond (reject initial) (constantly nil)
        (accept initial) (constantly 0)
        :else (fn [input]
                (loop [state initial
                       input (seq input)
                       counter 0]
                  (if-not (seq input)
                    (when (accept state) counter)
                    (let [n (next-state transitions state (first input))]
                      (cond (reject n) nil
                            (accept n) (inc counter)
                            :else (recur n (rest input) (inc counter)))))))))

(defn- fsm->total-count-fn
  "Generate counter function that counts the times the FSM enters an accepting state."
  [{:keys[initial transitions accept reject]}]
  (if (reject initial)
    (constantly nil)
    (fn [input]
      (loop [state initial
             input (seq input)
             counter (if (accept initial) 1 0)]
        (if-not (seq input)
          counter
          (let [n (next-state transitions state (first input))]
            (cond (reject n) counter
                  (accept n) (recur n (rest input) (inc counter))
                  :else (recur n (rest input) counter))))))))

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

