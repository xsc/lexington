(ns ^{ :doc "Checks for FSM Consistency"
       :author "Yannick Scherer" }
  lexington.fsm.consistency-check
  (:require [lexington.fsm.errors :as e]
            [lexington.fsm.helpers :as h]
            [lexington.fsm.states :as s]))

(defn- create-state-set
  "Create set of valid state names for a given list of FSM states."
  [states]
  (let [predefined-states #{(s/accept) (s/reject) (s/accept-ignore)}]
    (into predefined-states
          (reduce
            (fn [st {:keys[name] :as s}]
              (if (contains? st name)
                (e/duplicate-state name)
                (conj st name)))
            #{}
            states))))

(defn- check-states
  "Check FSM Consistency."
  [states]
  (let [valid-state? (create-state-set states)]
    (doseq [{:keys[name transitions] :as s} states]
      (doseq [[_ next-state] transitions]
        (when-not (valid-state? next-state)
          (e/transition-unknown-destination name next-state))))))
