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

(def eof
  "Evaluates to `:lexington.fsm.core/eof`. Will initiate a transition on end-of-
   data."
  (constantly ::eof))

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

;; ### FSM Generation

(defn- handle-special-states
  [state transitions]
  (reduce 
    (fn [t [k v]]
      (let [v (cond (= v (continue)) state
                    :else v)]
        (assoc t k v)))
    {}
    transitions))

(defn- generate-transition-table
  [states]
  (reduce (fn [table s]
            (let [nm (state-name s)]
              (if (get table nm)
                (throw (IllegalArgumentException. (str "Duplicate State:" nm)))
                (->> (handle-special-states nm (state-transitions s))
                  (assoc table nm)))))
          {}
          states))

(defn states->fsm 
  "Generate an FSM map from a sequence of state maps (created by `state`)."
  [states]
  (let [initial-state (state-name (first states))
        transitions (generate-transition-table states)
        state-keys (keys transitions)]
    (-> {}
      (assoc :initial     initial-state)
      (assoc :states      (set state-keys))
      (assoc :transitions transitions))))

;; ## FSM Instantiation

;; ### Recognizer (stateless)
;; A "stateless" FSM will run until one of the final states (accept/ignore-and-accept/reject)
;; is reached. It is a function that takes an input and returns the number of input elements
;; it recognized or nil.

(defn recognizer
  [{:keys [initial transitions] :as fsm}]
  (letfn [(next-state [current-state input]
            (if-let [new-state (or (get-in transitions [current-state input])
                                   (get-in transitions [current-state (any)]))]
              new-state
              (reject)))]
    (let [has-accepted? #{(accept) (ignore-and-accept)}
          has-rejected? #{(reject)}]
        (fn [in-seq]
          (loop [state initial
                 sq    in-seq
                 c     0]
            (cond (has-accepted? state) c
                  (has-rejected? state) nil
                  (not (seq sq)) (when (has-accepted? (next-state state (eof))) c)
                  :else (when-let [s (next-state state (first sq))]
                          (recur s (rest sq) (if (= s ::ignore-and-accept) c (inc c))))))))))

;; ### FSM Instances (stateful)

(defn run
  [fsm]
  (let [initial-map { :state (:initial-state fsm) 
                      :input nil }
        current-state (atom initial-map)
        transitions (:transition-table fsm)]
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
