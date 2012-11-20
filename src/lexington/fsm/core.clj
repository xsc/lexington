(ns ^{ :doc "Lexington FSM Representation"
       :author "Yannick Scherer" }
  lexington.fsm.core
  (:require [lexington.fsm.states :as s]
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

(defn fsm
  "Create new FSM map."
  [& states]
  (fsm/states->fsm states))

;; ## Macros


