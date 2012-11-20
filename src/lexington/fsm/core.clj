(ns ^{ :doc "Lexington FSM Representation"
       :author "Yannick Scherer" }
  lexington.fsm.core
  (:require [lexington.fsm.states :as s]
            [lexington.fsm.transitions :as t]
            [lexington.fsm.errors :as e]
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

(defn new-fsm
  "Create new FSM from a list of states."
  [& states]
  (fsm/states->fsm states))

;; ## fsm*
;;
;; The `fsm*` macro introduces shorthands for the different special transitions and target states:
;; 
;; - `*` represents the `(any)` transition
;; - `(:! ...)` represents the `(except ...)` transition
;; - `(:? ...)` represents the `(one-of ...)` transition
;; - `accept!` represents the `(accept)` destination state
;; - `reject!` represents the `(reject)` destination state
;; - `_` represents the `(continue)` destination state
;;
;; Additionally, it handles the creation of accepting/rejecting/normal states by using special 
;; keywords, and tries to increase readability by using "->" between input entity and destination
;; state:
;;
;;     (fsm* 
;;       (:state :a
;;         \a -> _
;;         \b -> accept!
;;         \c -> :c
;;         *  -> :r)
;;       (:accepting :c
;;         \c -> :a)
;;       (:rejecting :r))
;;
;; This will expand to:
;;
;;     (states->fsm [ 
;;       (new-state :a [ \a (continue) 
;;                       \b (accept) 
;;                       \c :c 
;;                       (any) :r ])
;;       (new-accept-state :c [ \c :a ]) 
;;       (new-reject-state :r) ])
;;
;; The FSM will accept languages matching the regular expression "a*(cca*)*b", of whatever use
;; that might be.

(defmacro fsm*
  "Generate FSM from a list of states with special keywords and symbols, e.g.:

    (fsm* 
      (:state :a
        (:! 'a 'b 'd) -> :c
        'a            -> _
        'b            -> accept!
        *             -> :r)
      (:accepting :c
        'c -> :a)
      (:rejecting :r))
  "
  [& states]
  (letfn [(get-name [k]
            (and (or (keyword? k)
                     (symbol? k))
                 (name k)))
          (convert-transitions [n transitions]
            (vec 
              (mapcat 
                (fn [[input arrow next-state]]
                  (when (not (= (get-name arrow) "->"))
                    (e/error "Missing '->' in transition [" input " -> ...] of state " n))
                  (vector (if (coll? input)
                            (let [[d & args] input
                                  sym (get-name d)]
                              (cond (= sym "!") `(t/except ~@args)
                                    (= sym "?") `(t/one-of ~@args)
                                    :else input))
                            (let [sym (get-name input)]
                              (cond (= sym "*") `(t/any)
                                    :else input)))
                          (let [sym (get-name next-state)]
                            (cond (= sym "_") `(s/continue)
                                  (= sym "accept!") `(s/accept)
                                  (= sym "reject!") `(s/reject)
                                  :else next-state))))
                (partition 3 transitions))))]
    (let [states* (map (fn [s]
                         (if-not (coll? s) s
                           (let [[k & [n & t]] s
                                  k (get-name k)]
                             (cond (= k "state")  `(s/new-state ~n ~(convert-transitions n t))
                                   (= k "accepting") `(s/new-accept-state ~n ~(convert-transitions n t))
                                   (= k "rejecting") `(s/new-reject-state ~n)
                                   :else s))))
                       states)]
      `(fsm/states->fsm ~(vec states*)))))
