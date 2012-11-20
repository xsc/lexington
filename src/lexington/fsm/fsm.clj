(ns ^{ :doc "Lexington FSM Generation"
       :author "Yannick Scherer" }
  lexington.fsm.fsm
  (:require [lexington.fsm.states :as s]
            [lexington.fsm.helpers :as h]))

;; ## FSM Generation
;;
;; A FSM is represented as a map with the following keys:
;;
;; - `:states`      : a set of all the states available
;; - `:initial`     : the starting state
;; - `:accept`      : a set of all states that let the FSM accept the input (if applicable)
;; - `:reject`      : a set of all states that let the FSM reject the input (if applicable)
;; - `:transitions` : a nested hash map `{ :current-state { <input> :next-state } }`
;;
;; Lexington takes a list of separate state maps (containing implicit references like 
;; `(continue)` or `(accept)`) and transforms it into the above representation, producing 
;; an unambiguous and inherently consistent FSM description.

(declare normalize-states
         generate-transitions)

(defn states->fsm
  "Add states to given FSM or create new FSM based on the given states. The first
   element of the list is used as the initial state if none was given so far."
  ([new-states] (states->fsm {} new-states))
  ([{:keys[initial states accept reject transitions] :as fsm} new-states]
   (let [states*       (normalize-states new-states)
         initial*      (s/state-name (first states*))
         state-set     (s/state-names states*)
         accept*       (s/state-names (filter s/accepting? states*))
         reject*       (s/state-names (filter s/rejecting? states*))
         transitions*  (generate-transitions states*)]
     (-> fsm
       (assoc :initial (or initial initial*))
       (assoc :states (set (into states state-set)))
       (assoc :accept (set (into accept accept*)))
       (assoc :reject (set (into reject reject*)))
       (assoc :transitions (merge transitions transitions*))))))

(defn ->fsm
  "Add states to given FSM."
  [fsm & states]
  (states->fsm fsm states))

;; ## Transition Table

(defn- generate-transitions
  "Generate transition table from a list of states. The result will be a nested map
   `{ <state> { <input> <next state> ... } ... }`" 
  [states]
  (reduce 
    (fn [table {:keys [name transitions] :as s}]
      (assoc table name transitions))
    {}
    states))

;; ## State Normalization
;; 
;; When generated using the functions in `lexington.fsm.states`, the resulting state maps might 
;; contain references like `(continue)` or `(reject)`. Normalization resolves these references.
;;
;; - `continue` will be replaced with the current state
;; - `reject` will be replaced with the name of a newly generated reject state
;; - `accept` will be replaced with the name of a newly generated accept state
;;

(defn- generate-state-name
  "Generate name for dynamically generated states. Namespacing is used to
   prevent name clashes, but I'm not sure if this is enough."
  [base current-state input]
  (let [input-str (if (keyword? input) 
                    (name input)
                    (str input))
        n (str (name current-state) "-"  (name base) "-on-" input-str)]
      (keyword "lexington.fsm.states" n)))

(defn- generate-acceptor-name
  "Generate name for accepting state."
  [current-state input]
  (generate-state-name :accept current-state input))

(defn- generate-rejector-name
  "Generate name for rejecting state."
  [current-state input]
  (generate-state-name :reject current-state input))

;; ### Reference Resolvers

(defn- resolve-continue
  "Replace `(continue)` transitions with the state's name."
  [{:keys[name] :as s}]
  (-> s
    (h/remap-in :transitions #(if (= % (s/continue)) name %))))

(defn- resolve-acceptors
  "Replace `(accept)` transitions with the name of a newly generated accepting
   state."
  [{:keys[name] :as s}]
  (-> s
    (h/map-in :transitions
      (fn [input next-state]
        (if (s/acceptor? next-state)
          (generate-acceptor-name name input)
          next-state)))))

(defn- resolve-rejectors
  "Replace `(reject)` transitions with the name of a newly generated rejecting state."
  [{:keys[name] :as s}]
  (-> s
    (h/map-in :transitions
      (fn [input next-state]
        (if (s/rejector? next-state)
          (generate-rejector-name name input)
          next-state)))))

;; ### State Generators

(defn- generate-acceptors
  "Generate accepting states when needed."
  [{:keys[name transitions]}]
  (filter (comp not nil?)
          (map (fn [[input next-state]]
                 (when (s/acceptor? next-state)
                   (s/new-accept-state (generate-acceptor-name name input) [])))
               transitions)))

(defn- generate-rejectors
  [{:keys[name transitions]}]
  "Generate rejecting states when needed."
  (filter (comp not nil?)
          (map (fn [[input next-state]]
                 (when (s/rejector? next-state)
                   (s/new-reject-state (generate-rejector-name name input))))
               transitions)))

;; ### Normalization

(defn- normalize-single-state
  "Normalize a given state. This function produces a vector of states which shall be used in place
   of the original state."
  [{ :keys[name transitions] :as s}]
  (cons
    (-> s
      resolve-continue
      resolve-acceptors
      resolve-rejectors
      (h/into-map-in :transitions))
    (concat
      (generate-acceptors s)
      (generate-rejectors s))))

(defn- normalize-states
  "Perform normalization on a list of states."
  [states]
  (mapcat normalize-single-state states))
