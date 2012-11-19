(ns ^{ :doc "Lexington FSM Representation"
       :author "Yannick Scherer" }
  lexington.fsm.core
  (:use [lexington.fsm.transitions :as t]
        lexington.fsm.states))

;; ## FSM Infrastructure
;;
;; ### Special Transitions

(def eof
  "Will initiate a transition on end-of-data."
  (constantly ::eof))

(def any
  "Will initiate a transition not depending on the input."
  (constantly t/*any-input*))

(defn except
  "Transition Rule that will initiate a transition only if the input does not match
  the values given."
  [& args]
  (when (seq args)
    (fn [s]
      (cons [t/*any-input* s]
            (map (fn [x]
                   [x nil]) args)))))

(defn one-of
  "Transition Rule that will initiate a transition only if the input does match one 
   of the values given."
  [& args]
  (set args))

;; ### Special Target States

(def continue
  "Stay in current state."
  (constantly ::continue))

(def reject
  "Let Recognition fail."
  (constantly t/*default-state*))

(def accept-ignore
  "Final state that ignores the next entity and accepts everything before it."
  (constantly ::accept-ignore))

(def accept
  "Final state that accepts everything up to the current entity."
  (constantly ::accept))

;; ### Map Helper Functions

(defn- remap
  "Apply function to each value in a map, producing a map seq
   `[ [key (f v1)] ... ]`"
  [m f]
  (map (fn [[k v]] 
         (vector k (f v))) m))

(defn- remap-in
  "Apply remap function to a map field."
  [m k f]
  (assoc m k (remap (get m k) f)))

(defn into-map-in
  "Convert map field's value (a map sequence) to map."
  [m k]
  (assoc m k (into {} (get m k))))

;; ### FSM Generation

(defn state
  "Syntactic sugar to create state map."
  [k & transitions]
  (generate-state k transitions))

(defn- check-states
  "TODO: Check FSM Consistency."
  [states]
  nil)

(defn- normalize-states
  "Create a state object with resolved implicit references (e.g. to the state itself)."
  [states]
  (let [states (map (fn [{ :keys[name transitions] :as state }]
                      (-> state
                        (remap-in :transitions #(if (= % ::continue) name %))
                        (into-map-in :transitions))) states)]
    (check-states states)
    states))

(defn- generate-transition-table
  "Generate transition table { <entry> <next state>, ... } from list
   of states."
  [states]
  (reduce 
    (fn [table s]
      (assoc table (:name s) (:transitions s)))
    {}
    states))

(defn states->fsm 
  "Generate an FSM map from a sequence of state maps (created by `state`)."
  [states]
  (let [states (normalize-states states)
        initial-state (state-name (first states))
        transitions (generate-transition-table states)
        state-keys (keys transitions)]
    (-> {}
      (assoc :initial     initial-state)
      (assoc :states      (set state-keys))
      (assoc :transitions transitions))))
