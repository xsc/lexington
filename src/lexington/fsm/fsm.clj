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
;; - `:meta`        : a nested hash map `{ :key { :state <data> } ... }` for actions, ...
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
       (assoc :meta {})
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
;;

;; ### Reference Resolvers

(defn- resolve-continue
  "Replace `(continue)` transitions with the state's name."
  [{:keys[name] :as s}]
  (-> s
    (h/remap-in :transitions #(if (= % s/continue!) name %))))

;; ### Normalization

(defn- normalize-single-state
  "Normalize a given state. This function produces a vector of states which shall be used in place
   of the original state."
  [{ :keys[name transitions] :as s}]
  (-> s
    resolve-continue
    (h/into-map-in :transitions)
    vector))

(defn- normalize-states
  "Perform normalization on a list of states."
  [states]
  (concat
    (mapcat normalize-single-state states)
    (vector (s/new-reject-state s/reject!)
            (s/new-accept-state s/accept! []))))
