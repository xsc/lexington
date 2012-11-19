(ns ^{ :doc "State Infrastructure for Lexington FSMs"
       :author "Yannick Scherer" }
  lexington.fsm.states
  (:use lexington.fsm.transitions))

;; A state is a map consisting of two entries, `:name` (the state's name) and
;; `:transitions` (the transition table). The latter one is again a map whose
;; keys are possible input entities, completed by the special key `::any` which
;; declares the default behaviour.

(defn generate-state
  "Create a single state map using a state name and a list of transitions. Example:

      (generate-state :k
        [ 'a             :a
          (except 'b 'c) :d
          (except 'b)    :c
          (any)          :b ])

      ;-> { :transitions { 'a :a, 
      ;                    :lexington.fsm.transitions/any :d, 
      ;                    'c :c, 
      ;                    'b :b } 
      ;->   :name :k }
  "
  [k transitions]
  (-> {}
    (assoc :name k)
    (assoc :transitions (transitions->map transitions))))

(defn state-name 
  "Get name of state." 
  [s]
  (:name s))

(defn state-transitions 
  "Get transitions of state."
 [s]
 (:transitions s))

