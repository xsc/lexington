(ns ^{ :doc "Lexington FSM Representation"
       :author "Yannick Scherer" }
  lexington.fsm.core
  (:require [lexington.fsm.states :as s]
            [lexington.fsm.transitions :as t]
            [lexington.fsm.errors :as e]
            [lexington.fsm.fsm :as fsm]))

;; ## Macros
;;
;; ### state/accept/reject

(defmacro transitions*
  "The `transitions*` macro can be used for state generation. It introduces shorthands for the 
different special transitions and target states:

- `(:not ...)` represents the `(except ...)` transition
- `(:or ...)` represents the `(one-of ...)` transition
- `:accept!` represents the `(accept)` destination state
- `:reject!` represents the `(reject)` destination state
- `_` represents the `(any) transition or the `(continue)` destination state
Additionally, the input entity and the next state are now separated by an arrow `->`. Example:
    
    (transitions*
      (:not 'a 'b 'c) -> :a
      (:or 'b 'c)     -> :accept!
      _               -> :f)

  "
  [& transitions]
  (letfn [(is-underscore? [x]
            (and (symbol? x) (= (name x) "_")))
          (is-arrow? [x]
            (and (symbol? x) (= (name x) "->")))
          (resolve-input [i]
            (if (coll? i)
              (let [[d & args] i]
                (cond (= d :not) `(t/except ~@args)
                      (= d :or) `(t/one-of ~@args)
                      :else i))
              (if (is-underscore? i) 
                `(t/any)
                i)))
          (resolve-destination [d]
            (cond (is-underscore? d) `(s/continue)
                  (= d :accept!) `(s/accept)
                  (= d :reject!) `(s/reject)
                  :else d))]

    (if (and (pos? (count transitions)) (< (count transitions) 3))
      (e/transition-missing-arrow "" (first transitions))
      (doseq [[i arrow _] (partition 3 transitions)]
        (when-not (is-arrow? arrow)
          (e/transition-missing-arrow "" i))))

    `(vector ~@(mapcat 
                 (fn [[input arrow next-state]]
                   (vector (resolve-input input)
                           (resolve-destination next-state)))
                     (partition 3 transitions)))))

(defmacro state
  "Create new state, passing its transitions to `transitions*` first."
  [k & transitions]
  `(s/new-state ~k  (transitions* ~@transitions)))

(defmacro accept-state
  "Create new accepting state."
  [k & transitions]
  `(s/new-accept-state ~k (transitions* ~@transitions)))

(defmacro reject-state
  "Create new rejecting state."
  [k]
  `(s/new-reject-state ~k))

;; ### with-default

(defmacro with-default
  "Sets default behaviour for a series of states."
  [k & states]
  `(vector ~@(map (fn [x]
                    `(binding [t/*default-state* ~k]
                       ~x)) states)))

;; ## FSM Creation Function

(defn fsm*
  "This function takes a list of single states or list of states and creates an FSM based on 
   the given states."
  [& states]
  (let [flattened-states
        (mapcat 
          (fn [x]
            (cond (map? x) (vector x)
                  (coll? x) x
                  :else (e/error "fsm* expects either a state or a collection of states as parameters.")))
          states)]
    (fsm/states->fsm flattened-states)))
