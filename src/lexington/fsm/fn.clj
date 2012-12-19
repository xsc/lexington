(ns ^{ :doc "Lexington FSM to function converters."
       :author "Yannick Scherer" }
  lexington.fsm.fn
  (:use [lexington.fsm.consts :as c]
        lexington.fsm.utils
        lexington.fsm.core))

;; ## Basic FSM Execution

;; ### NFA Execution
;; 
;; Each FSM can be executed in a non-deterministic way by maintaining a set of all the states
;; an FSM is currently in and producing the next states by applying the input to each one.

(defn nfa-execute
  "Execute the given NFA on the given sequence using a set of current states. This 
   function produces a lazy sequence of the generated state sets. The resulting seq
   contains the initial state set as well
  
   __Note:__ Expects a normalized NFA!"
  ([{:keys [initial] :as nfa} input-seq] 
   (nfa-execute nfa #{initial} input-seq))
  ([{:keys [transitions reject] :as nfa} current-states input-seq]
   (let [reject? (set reject)]
     (lazy-seq
       (cons current-states
             (when-let [[in & rst] (seq input-seq)]
               (let [next-states (->> current-states
                                   (mapcat #(fsm-destination-states nfa % in))
                                   set)]
                 (nfa-execute nfa next-states rst))))))))

(defn nfa-execute-until-reject
  "Execute the given NFA on the given sequence using a set of current states. This
   function produces a lazy sequence of the generated state sets (without rejecting
   states) until execution cannot continue.

   __Note:__ Expectes a normalized NFA!"
  ([{:keys [initial] :as nfa} input-seq] 
   (nfa-execute-until-reject nfa #{initial} input-seq))
  ([{:keys [transitions reject] :as nfa} current-states input-seq]
   (let [reject? (set reject)]
     (lazy-seq
       (when-let [current-states (seq (filter (comp not reject?) current-states))]
         (cons 
           (set current-states)
           (when-let [[in & rst] (seq input-seq)]
             (let [next-states (->> current-states
                                 (mapcat #(fsm-destination-states nfa % in))
                                 (filter (comp not reject?)))]
               (nfa-execute-until-reject nfa next-states rst)))))))))

(defn nfa-count
  "Produce a lazy sequence of the number of entities consumed to let the NFA reach
   at least one state matching the given predicate. (E.g. if the initial state matches
   the resulting seq will start with a `0` element.)
  
   __Note:__ Expects a normalized NFA."
  [nfa p? input-seq]
  (->>
    (nfa-execute-until-reject nfa input-seq)
    (map-indexed 
      (fn [i states]
        (when (some p? states)
          i)))
    (filter (comp not nil?))))

(defmacro defn-normalized
  "Define function where the first parameter is an auto-normalized FSM."
  [id metadata [sym & p] & body]
  `(defn ~id 
     ~metadata
     [fsm# ~@p]
    (when-not (nfa? fsm#)
      (throw (Exception. "Only applicable to NFAs.")))
     (let [~sym (fsm-normalize fsm#)]
       ~@body)))

;; ### Execution Function

(defn-normalized nfa-fn
  "Generate a function that will consume a lazy sequence of input entities and produce a 
   lazy sequence of the state sets reached after consuming an input entity."
  [nfa]
  (fn [input-seq]
    (nfa-execute nfa input-seq)))

;; ### Counter Function
;;
;; Let an NFA consume sequence of input entities and return a lazy sequence of the number
;; (not index!) of input elements that were consumed to let the NFA reach at least one 
;; state matching the given predicate.

(defn-normalized count-fn
  "Generate sequence producing a lazy sequence of the number (not index) of elements consumed
   where the NFA ended up in at least one state matching the given predicate."
  [nfa p?]
  (fn [input-seq]
    (nfa-count nfa p? input-seq)))

;; ### Language Recognition Function
;;
;; Let an NFA consume a sequence of input entities and check if the state set after the last
;; entity contains at least one accepting state. Return the accepting states or nil.

(defn-normalized recognize-fn
  "Generate a function that, given an input seq, will return the set of accepting states
   reached after consuming the whole seq, or nil."
  [{:keys [accept] :as nfa}]
  (fn [input-seq]
    (when-let [final-states (last (nfa-execute nfa input-seq))]
      (let [accepting-states (filter #(contains? accept %) final-states)]
        (when (seq accepting-states)
          (set accepting-states))))))

(defn-normalized prefix-count-fn
  "Generate a function that will return a lazy seq of the number of input entities matched by the 
   given NFA. If the initial state is already accepting, the resulting seq will start with a `0`
   element."
  [{:keys [accept] :as nfa}]
  (let [accept? (set accept)]
    (fn [input-seq]
      (nfa-count nfa accept? input-seq))))

(defn prefix-match-count-fn
  "Generate a function that will return the length of the matched prefix of the given input 
   sequence. It is possible to supply an optional `:match` parameter that determines what 
   kind of result is returned:
   - `:greedy` (default): match as much as possible
   - `:non-greedy`: match as little as possible
   - `:total`: return the total number of matchable sub-sequences.
  "
  [fsm & {:keys[matcher]}]
  (let [prefix-counter (prefix-count-fn fsm)]
    (fn [input-seq]
      (when-let [prefix-counts (prefix-counter input-seq)]
        (case matcher
          :non-greedy (first prefix-counts)
          :total (count prefix-counts)
          :greedy (last prefix-counts)
          (last prefix-counts))))))

(defn prefix-match-fn
  "Generate a function that will return the matched prefix of the given input sequence. An
   optional `:match` parameter can be supplied that will be passed to `prefix-match-count-fn`."
  [fsm & {:keys [match]}]
  (let [prefix-matcher (prefix-match-count-fn fsm :matcher match)]
    (fn [input-seq]
      (when-let [c (prefix-matcher input-seq)]
        (take c input-seq)))))
