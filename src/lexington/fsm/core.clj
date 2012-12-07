(ns ^{ :doc "Lexington FSM Representation"
       :author "Yannick Scherer" }
  lexington.fsm.core
  (:require [lexington.fsm.consts :as c]) 
  (:use [lexington.fsm.errors :as e]
        lexington.fsm.utils))


;; ## FSM Structure
;;
;; A FSM is represented as a map with the following keys:
;;
;; - `:states`      : a set of all the states available
;; - `:initial`     : the starting state
;; - `:accept`      : a set of all states that let the FSM accept the input (if applicable)
;; - `:reject`      : a set of all states that abort recognition immediately
;; - `:transitions` : a nested hash map `{ :current-state { <input> #{<next-states>} } }`
;; - `:type`        : the FSM type (`:e-nfa`, `:nfa`, `:dfa`)
;;
;; The FSM type can be determined by examining the FSM alphabet and target state sets: if the
;; alphabet contains `lexington.fsm.core/epsi` it's `:e-nfa`; if there is one target state set with
;; more than one element it's `:nfa`; otherwise the FSM is a `:dfa`. All lexington operations will 
;; adjust the type of the FSM when needed.


;; ### Validation of FSM Types

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
  [{:keys[transitions]}]
  (->>
    (vals transitions)
    (mapcat vals)
    (map count)
    (some #(> % 1))
    not))

(defn fsm-type
  "Derive FSM type from structure of FSM."
  [fsm]
  (letfn [(is-fsm? [t]
            (when (-> fsm
                    (assoc :type t)
                    validate-fsm-type)
              t))]
    (or 
      (is-fsm? :dfa)
      (is-fsm? :nfa)
      (is-fsm? :e-nfa))))

;; ### Add Transition

(defmulti add-fsm-transition
  "Add Transition to FSM based on FSM type."
  (fn [{:keys[type]} from input to]
    type))

(defn- add-nfa-transition
  "Add transition to NFA by merging target state sets."
  [{:keys[transitions] :as fsm} from input to]
  (let [tt (get-in transitions [from input])]
    (assoc-in fsm 
              [:transitions from input]
              (conj (set tt) to))))

(defmethod add-fsm-transition :e-nfa
  [fsm from input to]
  (add-nfa-transition fsm from input to))

(defmethod add-fsm-transition :nfa
  [fsm from input to]
  (when-not (= input c/epsi)
    (add-nfa-transition fsm from input to)))

(defmethod add-fsm-transition :dfa
  [{:keys[transitions] :as fsm} from input to]
  (when (empty? (set (get-in transitions [from input])))
    (when-not (= input c/epsi)
      (assoc-in fsm [:transitions from input] (hash-set to)))))

;; ### FSM Creation

(defn add-transition
  "Add transition (and its states) to FSM."
  [{:keys[type states initial] :as fsm} from input to]
  (if-let [fsm* (add-fsm-transition fsm from input to)]
    (-> fsm*
      (assoc :states (conj (conj (set states) from) to))
      (assoc :initial (or initial from)))
    (e/error
      (str 
        (format "  Transition [%s -> %s] in state `%s' not allowed.\n" input to from)
        (format "  FSM is of type `%s'. " type)
        (when-let [t (get-in fsm [:transitions from])]
          (format "Current transitions in `%s': %s" from t))))))

(defn add-transition-set
  "Add transition to a set of states to FSM."
  [fsm from input to-set]
  (reduce
    (fn [fsm to]
      (add-transition fsm from input to))
    fsm
    to-set))

(defn add-transitions
  "Add multiple transitions with a common source state, e.g.:

    (add-transitions fsm 
      [[:a 0 :a 1 :b 2 :c]
       [:b 1 :b 2 :c]
       [:c 2 :c]])

   This is the same as:

    (-> fsm
      (add-transition :a 0 :a)
      (add-transition :a 1 :b)
      (add-transition :a 2 :c)
      (add-transition :b 1 :b)
      (add-transition :b 2 :c)
      (add-transition :c 2 :c))
  "
  [fsm transitions]
  (reduce 
    (fn [fsm [from & targets]]
      (reduce
        (fn [fsm [input to]]
          (add-transition fsm from input to))
        fsm
        (partition 2 targets)))
    fsm
    transitions))

(defn fsm-creator
  "Will create a function that generates FSMs based on a list of transitions
   to be passed to `add-transitions`. Lists are given inline:
  
    (def x (fsm-creator :x))
    ...
    (x [:a 0 :a 1 :b]
       [:b 1 :b])
  "
  [type]
  (let [initial-fsm (-> {}
                      (assoc :type type)
                      (assoc :accept #{})
                      (assoc :reject #{})
                      (assoc :states #{})
                      (assoc :initial nil)
                      (assoc :transitions {}))]
  (fn [& transitions]
    (add-transitions initial-fsm transitions))))

;; ### e-NFA/NFA/DFA

(def epsilon-nfa*
  "Create new epsilon-NFA."
  (fsm-creator :e-nfa))

(defn epsilon-nfa?
  [{:keys[type]}]
  (= type :e-nfa))

(def nfa*
  "Create new non-epsilon NFA."
  (fsm-creator :nfa))

(def nfa?
  "Is the FSM a non-epsilon-NFA?
   Note that DFAs are NFAs to."
  (comp not epsilon-nfa?))

(def dfa*
  "Create new DFA."
  (fsm-creator :dfa))

(defn dfa?
  [{:keys[type]}]
  (= type :dfa))

;; ### Common Operations

(defn accept-in
  "Add accepting state to FSM."
  [{:keys[initial] :as fsm} & state-list]
  (->
    (reduce 
      (fn [{:keys[accept reject states] :as fsm} state]
        (if-not state
          fsm
          (-> fsm
            (assoc :reject (disj (set reject) state))
            (assoc :accept (conj (set accept) state))
            (assoc :states (conj (set states) state)))))
      fsm
      state-list)
    (assoc :initial (or initial (first state-list)))))

(defn accept-empty
  "Let the FSM accept empty inputs."
  [{:keys[initial] :as fsm}]
  (accept-in fsm initial))

(defn reject-in 
  "Add rejecting state to FSM."
  [{:keys[initial] :as fsm} & state-list]
  (->
    (reduce 
      (fn [{:keys[accept reject states] :as fsm} state]
        (if-not state
          fsm
          (-> fsm
            (assoc :reject (conj (set reject) state))
            (assoc :accept (disj (set accept) state))
            (assoc :states (conj (set states) state))
            (assoc-in [:transitions state] { c/any #{state} }))))
      fsm
      state-list)
    (assoc :initial (or initial (first state-list)))))

(defn initial-state
  "Set initial state of FSM."
  [{:keys[initial states] :as fsm} state]
  (if-not state
    fsm
    (-> fsm
      (assoc :initial state)
      (assoc :states (conj (set states) state)))))

;; ## Transition DSL
;; 
;; Transitions are given as `<input> -> <state(s)>` pairs, where inputs are two-element
;; vectors whose first elements have the following semantics:
;;
;; - `:one-of` : value is a seq of further input vectors; match if one matches
;; - `:except` : value is a seq of further input vectors; match if none matches
;; - `:literal`: value is anything; match if values are equal
;; - `:epsilon`: match epsilon
;; - `:any`    : match anything
;;
;; Target states are given as a set. 

;; ### Input Shorthands
;;
;; It is of course possible to have shorthands to be converted into suitable vectors:
;;
;; - `#{...}` will be converted to `[:one-of #{...}]`
;; - every non-vector will be converted to `[:literal x]`

(defprotocol ^:private TransitionInput
  (input->vector [i]
    "Convert input entity to vector usable by transition
     map generator."))
                 
(extend-protocol TransitionInput
  clojure.lang.IPersistentSet
  (input->vector [i]
    (vector :one-of i))
  clojure.lang.IPersistentVector
  (input->vector [i] 
    i)
  clojure.lang.ISeq
  (input->vector [i]
    (vector :one-of i))
  java.lang.Object
  (input->vector [i]
    (vector :literal i))
  nil
  (input->vector [i]
    (vector :literal i)))

(defn except
  "Create transition input that matches only if the entity is not
   in the given sequence."
  [inputs]
  (vector :except 
          (set inputs)))

(defn except*
  "Create transition input that matches only if the entity is not
   one of the given values."
  [& inputs]
  (except inputs))

(defn one-of
  "Create transition input that matches only if the entity is in
   the given sequence."
  [inputs]
  (vector :one-of
          (set inputs)))

(defn one-of*
  "Create transition input that matches only if the entity is one 
   of the given values."
  [& inputs]
  (except inputs))

(def ^:const eps c/epsi)

;; ### Target State Shorthands

(defprotocol ^:private TargetStates
  (target->set [t]
    "Convert target state representation into target state set."))

(extend-protocol TargetStates
  clojure.lang.IPersistentSet
  (target->set [t]
    t)
  java.lang.Object
  (target->set [t]
    #{t})
  nil
  (target->set [t]
    #{}))

;; ### Generator

(defn- generate-fsm-transitions
  "Generate FSM transitions from a seq of [input target] pairs where `input`
   is a vector (consisting of `:one-of`, `:except`, `:literal`, `:epsilon`,  
   `:any` and others, as well as additional data when needed) and `target` is a 
   set of destination states.
  
   Transitions are processed in the given order, which only has significance for 
   `:any` (all remaining transitions will be ignored), as well as `:except` transition
   which cannot override or extend previously set targets.
   "
  [fsm src-state pairs]
  (loop [fsm fsm
         handled-inputs #{}
         any-inputs #{c/any}
         pairs pairs]
    (if-not (seq pairs)
      (reduce 
        #(add-transition %1 src-state %2 c/reject!)
        fsm
        any-inputs)
      (let [[[input target] & rst] pairs
            [k data] (if (keyword? input) [input] input)
            add-tr (fn [fsm i]
                     (add-transition-set fsm src-state i target))]
        (case k
          :literal (recur (add-tr fsm data) 
                          (conj handled-inputs data)
                          any-inputs 
                          rst)
          :one-of  (recur (reduce add-tr fsm data) 
                          (set (concat data handled-inputs))
                          any-inputs 
                          rst)
          :epsilon (recur (add-tr fsm c/epsi) 
                          (conj any-inputs c/epsi)
                          any-inputs 
                          rst)
          :except (let [inputs-to-handle (filter 
                                           (comp not (set (concat handled-inputs data)))
                                           any-inputs)]
                    (recur (reduce add-tr fsm inputs-to-handle)
                           (set (concat inputs-to-handle handled-inputs))
                           (set (filter (comp not handled-inputs) data))
                           rst))
          :any (if (seq any-inputs)
                 (reduce add-tr fsm any-inputs)
                 fsm)
          (recur fsm handled-inputs any-inputs rst))))))

(defn- normalize-fsm-transitions
  "Resolve shorthands in input/target pairs destined for `generate-fsm-transitions`."
  [pairs]
  (map
    (fn [[input target]]
      (vector
        (input->vector input)
        (target->set target)))
    pairs))

(defn- generate-state
  "Generate new state in FSM based on a state name and a list of transition pairs to
   be eventually processed by `generate-fsm-transitions`. If there is already a state
   with the given name, it might be overwritten."
  [{:keys[states] :as fsm} state pairs]
  (let [pairs (normalize-fsm-transitions pairs)]
    (generate-fsm-transitions fsm state pairs)))

;; ### FSM Directives

(defmulti extend-fsm
  "An FSM is created using a sequence of directives like the following:

     [:state  :s ...]
     [:accept :a ...]
     ...

  This function takes one such directive and applies it to an FSM. To implement
  new directives, one has to implement this multimethod, as well as `form->directive`."
  (fn [k & _]
    k))

(defmethod extend-fsm :state
  [_ fsm s t]
  (-> fsm
    (generate-state s t)))

(defmethod extend-fsm :accept
  [_ fsm s t]
  (-> fsm
    (generate-state s t)
    (accept-in s)))

(defmethod extend-fsm :reject
  [_ fsm s t]  
  (-> fsm
    (generate-state s t)
    (reject-in s)))

(defmethod extend-fsm nil
  [_ fsm & _]
  fsm)

(defn extend-fsm-with-directives
  "Extend FSM by applying the given directives sequentially."
  [fsm & directives]
  (reduce
    (fn [fsm [k & r]]
      (apply extend-fsm k fsm r))
    fsm
    directives))

(defmulti form->directive
  "Convert a form like

     (:state :s 0 -> :q)

   into its directive representation, e.g.:

     [:state :s [[0 :q]]]

   To add new directives one has to implement this multimethod, as well as
   `extend-fsm`."
  (fn [[k & _]]
    k)
  :default :state)

(defn- normalize-transitions
  "Convert 'fancy' transitions to a normalized vector, e.g.

    (normalize-transitions :x [0 -> :a 1 -> :b])

   will yield:

    [[0 :a] [1 :b]]
  "
  [state transitions]
  (letfn [(is-underscore? [x]
            (and (symbol? x) (= (name x) "_")))
          (is-arrow? [x]
            (and (symbol? x) (= (name x) "->")))
          (resolve-input [i]
            (if (is-underscore? i) 
              `[:literal c/any]
              i))
          (resolve-destination [d]
            (cond (is-underscore? d) `(hash-set ~state)
                  :else d))]
    (if (and (pos? (count transitions)) (< (count transitions) 3))
      (e/transition-missing-arrow "" (first transitions))
      (doseq [[i arrow _] (partition 3 transitions)]
        (when-not (is-arrow? arrow)
          (e/transition-missing-arrow "" i))))
    `(vector
       ~@(map
           (fn [[input _ to]]
             (vector (resolve-input input)
                     (resolve-destination to)))
           (partition 3 transitions)))))

(defmethod form->directive :state
  [[k state & transitions]]
  [k state (normalize-transitions state transitions)])

;; ## FSM Generator Macros

(defmacro fsm*
  "Create FSM using a base FSM and a series of directives."
  [base & directives]
  `(extend-fsm-with-directives ~base
     ~@(map form->directive directives)))

(defmacro epsilon-nfa
  "Create new e-NFA using the given directives."
  [& directives]
  `(e/with-error-meta ~(meta &form)
     (fsm* (epsilon-nfa*) ~@directives)))

(defmacro nfa
  "Create new NFA using the given directives."
  [& directives]
  `(e/with-error-meta ~(meta &form)
     (fsm* (nfa*) ~@directives)))

(defmacro dfa
  "Create new DFA using the given directives."
  [& directives]
  `(e/with-error-meta ~(meta &form)
     (fsm* (dfa*) ~@directives)))

(defmacro def-epsilon-nfa
  "Define new e-NFA."
  [id & directives]
  `(e/with-error-source ~(str id)
     (e/with-error-meta ~(meta &form)
       (def ~id (fsm* (epsilon-nfa*) ~@directives)))))

(defmacro def-nfa
  "Define new NFA."
  [id & directives]
  `(e/with-error-source ~(str id)
     (e/with-error-meta ~(meta &form)
       (def ~id (fsm* (nfa*) ~@directives)))))

(defmacro def-dfa
  "Define new DFA."
  [id & directives]
  `(e/with-error-source ~(str id)
     (e/with-error-meta ~(meta &form)
       (def ~id (fsm* (dfa*) ~@directives)))))
