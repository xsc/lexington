(ns ^{ :doc "Validation of Lexington FSM Types"
       :author "Yannick Scherer" }
  lexington.fsm.validate-types
  (:use [lexington.fsm.utils :only [fsm-alphabet]]
        [lexington.fsm.consts :as c :only [epsi]]
        [lexington.fsm.errors :as e]))

;; ## Validation of FSM Types

(defmulti validate-fsm-type
  "Check if an FSM has the format its type requires."
  (fn [{:keys[type]}]
    type)
  :default :e-nfa)

(defmethod validate-fsm-type :e-nfa
  [{:keys[transitions]}]
  true)

(defmethod validate-fsm-type :nfa
  [fsm]
  (not (some #(= % c/epsi) (fsm-alphabet fsm))))

(defmethod validate-fsm-type :dfa
  [{:keys[transitions] :as fsm}]
  (let [target-counts (->> (vals transitions)
                        (mapcat vals)
                        (map count))
        alphabet (fsm-alphabet fsm)]
    (and (not (some #(= % c/epsi) alphabet))
         (not (some #(> % 1) target-counts)))))

;; ## Utilities

(defn fsm-set-type
  "Set type of FSM to the given value. This will perform a consistency check and throw
   an exception if the FSM's structure does not match the given type."
  [fsm t]
  (let [nfsm (assoc fsm :type t)]
    (if-not (validate-fsm-type nfsm)
      (e/error "Cannot set type of FSM to '" t "' since its structure would not match the given type."))
      nfsm))

(defn fsm-is?
  "Check if FSM is of the given type by examining its structure."
  [fsm t]
  (-> fsm
    (assoc :type t)
    validate-fsm-type))

(defn fsm-type
  "Derive FSM type from structure of FSM."
  [fsm]
  (letfn [(is-fsm? [t]
            (when (fsm-is? fsm t)
              t))]
    (or 
      (is-fsm? :dfa)
      (is-fsm? :nfa)
      (is-fsm? :e-nfa))))

