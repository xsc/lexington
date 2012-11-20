(ns ^{ :doc "State Infrastructure for Lexington FSMs"
       :author "Yannick Scherer" }
  lexington.fsm.states
  (:use [lexington.fsm.transitions :as t]))

;; ## Special Destination States

(def continue
  "Stay in current state."
  (constantly ::continue))

(defn reject
  "Let Recognition fail."
  []
  t/*default-state*)

(def accept
  "Final state that accepts everything up to the current entity."
  (constantly ::accept))

(defn acceptor?
  "Check if destination state is predefined acceptor."
  [s]
  (= s ::accept))

(defn rejector?
  "Check if destination state is predefined rejector."
  [s]
  (= s (reject)))

;; ## State Representation

(defn new-state
  "Create state map from a state name, a list of transitions.

    (new-state :k
      [ 'a             :a
        (except 'b 'c) :d
        (except 'b)    :c
        (any)          :b ])

    ;-> { :transitions { 'a :a, 
    ;                    :lexington.fsm.transitions/any :d, 
    ;                    'c :c, 
    ;                    'b :b },
    ;     :name :k }
  "
  [k transitions]
  (-> {}
    (assoc :name k)
    (assoc :transitions (t/transitions->map transitions))))

(defn state-name 
  "Get name of state." 
  [s]
  (:name s))

(defn state-transitions 
  "Get transitions of state."
 [s]
 (:transitions s))

(defn state-names
  "Get a set of state names from a list of states."
  [states]
  (set (map state-name states)))

;; ## State Modifiers

(defn accepting
  "Convert to accepting state."
  [state]
  (assoc state ::type ::accepting))

(defn rejecting
  "Convert to rejecting state."
  [state]
  (assoc state ::type ::rejecting))

(defn accepting?
  "Check if state is accepting."
  [state]
  (= (::type state) ::accepting))

(defn rejecting?
  "Check if state is rejecting."
  [state]
  (= (::type state) ::rejecting))

(defn new-accept-state
  "Create new accepting state."
  [k transitions]
  (-> (new-state k transitions)
    accepting))

(defn new-reject-state
  "Create new rejecting state."
  [k]
  (-> (new-state k [])
    rejecting))
