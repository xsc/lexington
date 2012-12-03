(ns ^{ :doc "State Infrastructure for Lexington FSMs"
       :author "Yannick Scherer" }
  lexington.fsm.states
  (:use [lexington.fsm.transitions :as t :only [nil-state]]))

;; ## Special Destination States

(def ^:const reject!
  "Let Recognition fail."
  t/nil-state)

(def ^:const accept!
  "Final state that accepts everything up to the current entity."
  ::accept)
