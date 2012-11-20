(ns ^{ :doc "Lexington FSM Representation"
       :author "Yannick Scherer" }
  lexington.fsm.core
  (:require [lexington.fsm.states :as s]
            [lexington.fsm.fsm :as fsm]))

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

(defn fsm
  "Create new FSM map."
  [& states]
  (fsm/states->fsm states))
