(ns ^{ :doc "Lexington FSM Representation"
       :author "Yannick Scherer" }
  lexington.fsm.core
  (:use lexington.fsm.transition-generator))

;; ## FSM Generator

;; ### Single State Generation
;;
;; A state is a map consisting of two entries, `::name` (the state's name) and
;; `::transitions` (the transition table). The latter one is again a map whose
;; keys are possible input entities, completed by the special key `::any` which
;; declares the default behaviour.

(declare any reject)

(defn state
  "Create a single state map using a state name and a list of transitions. Example:

      (state :k
        'a             :a
        (except 'b 'c) :d
        (except 'b)    :c
        (any)          :b)

      ;-> { ::transitions {'a :a, ::any :d, 'c :c, 'b :b} 
      ;->   ::name :k }
  "
  [k & transitions]
  (let [tmap (binding [*any-entity*    (any)
                       *default-state* (reject)]
               (transitions->map transitions))]
    (-> {}
      (assoc ::name k)
      (assoc ::transitions tmap))))

(defn state-name
  "Get the name of a state."
  [s]
  (::name s))

(defn state-transitions
  "Get the transition map of a state."
  [s]
  (::transitions s))

;; ### FSM Generation
;;

(defn states->fsm 
  "Generate an FSM map from a sequence of state maps (created by `state`)."
  [states]
  (let [initial-state (state-name (first states))
        state-table (reduce (fn [table s]
                              (-> table
                                (assoc (state-name s) (state-transitions s))))
                            {}
                            states)
        state-keys (keys state-table)]
    (-> {}
      (assoc ::initial-state initial-state)
      (assoc ::states (vec state-keys))
      (assoc ::transition-table state-table))))



;; ### FSM Instances (stateful)

(defn run
  [fsm]
  (let [initial-map { :state (::initial-state fsm) 
                      :input nil }
        current-state (atom initial-map)
        transitions (::transition-table fsm)]
    (letfn [(next-state [{:keys[state input] :as m} e]

              (if-let [t (or (get-in transitions [state e])
                             (get-in transitions [state (any)]))]
                (-> m
                  (assoc :state t)
                  (assoc :input (concat input [e])))
                m))]
      (fn [e]
        (cond (= e ::reset) (swap! current-state (fn [x] initial-map))
              :else (swap! current-state next-state e))))))


;;(defmacro fsm
;;  [& states]
;;  (let [state-maps (vec (filter (comp not nil?)
;;                                (map (fn [s]
;;                                       (eval `(state ~@(rest s)))) states)))]
;;    `(let [s# ~state-maps]
;;       s#)))


;; ### Special Destination States

(def continue
  "Stay in current state."
  (constantly ::continue))

(def reject
  "Let Recognition fail."
  (constantly ::reject))

(def ignore-and-accept
  "Final state that ignores the next entity and accepts everything before it."
  (constantly ::ignore-and-accept))

(def accept
  "Final state that accepts everything up to the current entity."
  (constantly ::accept))

;; ### Special Transition Rules

(def any
  "Syntactic Sugar. Evaluates to `:lexington.fsm.core/any`. Will initiate a transition
   not depending on the input."
  (constantly ::any))

(defn except
  "Transition Rule that will initiate a transition only if the input does not match
  the exceptions given."
  [& args]
  (when (seq args)
    (fn [s]
      (cons [::any s]
            (map (fn [x]
                   [x nil]) args)))))

(defn one-of
  "Transition Rule that will initiate a transition only if the input does match one of the
   values given."
  [& args]
  (when (seq args)
    (fn [s]
      (map #(vector % s) args))))
