(ns ^{ :doc "Lexington FSM Representation"
       :author "Yannick Scherer" }
  lexington.fsm.core
  (:require [lexington.fsm.states :as s]
            [lexington.fsm.transitions :as t]
            [lexington.fsm.errors :as e]
            lexington.fsm.utils))

;; ## FSM Structure
;;
;; A FSM is represented as a map with the following keys:
;; - `:states`      : a set of all the states available
;; - `:initial`     : the starting state
;; - `:accept`      : a set of all states that let the FSM accept the input (if applicable)
;; - `:transitions` : a nested hash map `{ :current-state { <input> #{<next-states>} } }`
;; Whether the FSM is a DFA or an NFA depends only on the contents of the target state sets:
;; if there is any set that contains more than one state, it's an NFA; DFA otherwise.

;; ### NFA

(def ^:const epsi ::epsilon)

(defn nfa-add
  "Add Transition to NFA."
  [{:keys[transitions states initial] :as nfa} from input to]
  (let [current-transitions (or (get-in transitions [from input]) #{})
        to-set (if (set? to) to (hash-set to))]
    (-> nfa
      (assoc-in [:transitions from input]
                (if (set? current-transitions)
                  (set (concat to-set current-transitions))
                  (conj to-set current-transitions)))
      (assoc :states (set (concat to-set (conj (set states) from))))
      (assoc :initial (or initial from)))))

(defn nfa-add-epsilon
  "Add Epsilon Transition to NFA."
  [nfa from to]
  (nfa-add nfa from epsi to))

(defn nfa*
  "Create NFA from a list of transition vectors. A transition vector
   consists of the source state and a list of input/next-state pairs."
  [& transitions]
  (reduce
    (fn [nfa [src-state & trv]]
      (let [pairs (partition 2 trv)]
        (reduce 
          (fn [nfa [input dst-state]]
            (nfa-add nfa src-state input dst-state))
          nfa
          pairs)))
    {}
    transitions))

;; ### DFA

(defn dfa-add
  "Add DFA transition. This will replace any existing transition for the
   given state and input."
  [{:keys[transitions states initial] :as dfa} from input to]
  (let [current-transitions (get-in transitions [from input])]
    (-> dfa
      (assoc-in [:transitions from input] (hash-set to))
      (assoc :states (conj (conj (set states) to) from))
      (assoc :initial (or initial from)))))

(defn dfa*
  "Create DFA from a list of transition vectors. A transition vector
   consists of the source state and a list of input/next-state pairs."
  [& transitions]
  (reduce
    (fn [dfa [src-state & trv]]
      (let [pairs (partition 2 trv)]
        (reduce 
          (fn [dfa [input dst-state]]
            (dfa-add dfa src-state input dst-state))
          dfa
          pairs)))
    {}
    transitions))

;; ### Check Type of FSM

(defn epsilon-nfa?
  "Check if a given FSM is an epsilon-NFA."
  [{:keys[transitions]}]
  (some #(= % epsi)
        (mapcat keys (vals transitions))))

(defn nfa?
  "Check if a given FSM is an NFA."
  [{:keys[transitions] :as fsm}]
  (or (epsilon-nfa? fsm)
      (some (comp seq rest) 
            (mapcat vals (vals transitions)))))

(defn dfa?
  "Check if a given FSM is a DFA."
  [fsm]
  (not (nfa? fsm)))

;; ### Common Operations

(defn accept-in
  "Add accepting state to FSM."
  [fsm & state-list]
  (reduce 
    (fn [{:keys[accept reject states] :as fsm} state]
      (if-not state
        fsm
        (-> fsm
          (assoc :reject (disj (set reject) state))
          (assoc :accept (conj (set accept) state))
          (assoc :states (conj (set states) state)))))
    fsm
    state-list))

(defn accept-empty
  "Let the FSM accept empty inputs."
  [{:keys[initial] :as fsm}]
  (accept-in fsm initial))

(defn reject-in 
  [fsm & state-list]
  (reduce 
    (fn [{:keys[accept reject states] :as fsm} state]
      (if-not state
        fsm
        (-> fsm
          (assoc :reject (conj (set reject) state))
          (assoc :accept (disj (set accept) state))
          (assoc :states (conj (set states) state)))))
    fsm
    state-list))

(defn initial-state
  "Set initial state of FSM."
  [{:keys[initial states] :as fsm} state]
  (if-not state
    fsm
    (-> fsm
      (assoc :initial state)
      (assoc :states (conj (set states) state)))))

;; ## Transition DSL

(defn nfa-combine 
  "Transition combination function for NFAs."
  [a b]
  (let [a-set (if (set? a) a (hash-set a))
        b-set (if (set? b) b (hash-set b))]
    (set (concat a-set b-set))))

(defn dfa-combine 
  "Transition combination function for DFAs."
  [a b] 
  a)

(defmacro state*
  "The `state*` macro can be used for state generation. It introduces shorthands for the 
different special transitions and target states:

- `(:not ...)` represents the `(except ...)` transition
- `(:or ...)` represents the `(one-of ...)` transition
- `:accept!` represents the `accept!` destination state
- `:reject!` represents the `reject!` destination state
- `_` represents the `any` transition or the `continue!` destination state
Additionally, the input entity and the next state are now separated by an arrow `->`. Example:
    
    (state* :dfa :state-a
      (:not 'a 'b 'c) -> :state-a
      (:or 'b 'c)     -> :accept!
      _               -> :state-f)

  "
  [type k & transitions]
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
                `t/any
                i)))
          (resolve-destination [d]
            (cond (is-underscore? d) k
                  (= d :accept!) `s/accept!
                  (= d :reject!) `s/reject!
                  :else d))]

    (if (and (pos? (count transitions)) (< (count transitions) 3))
      (e/transition-missing-arrow "" (first transitions))
      (doseq [[i arrow _] (partition 3 transitions)]
        (when-not (is-arrow? arrow)
          (e/transition-missing-arrow "" i))))

    `(mapcat vec
       (t/resolve-transitions 
         (vector ~@(mapcat 
                     (fn [[input arrow next-state]]
                       (vector (resolve-input input)
                               (resolve-destination next-state)))
                     (partition 3 transitions)))
         ~(if (= :dfa type) `dfa-combine `nfa-combine)))))

;; ## NFA/DFA generation
;;
;; Generation is done by supplying the states to use:
;;
;;     (nfa
;;       (:state :init
;;         \a -> :a
;;         \b -> :b)
;;       (:state :b 
;;         \a -> _)
;;       (:accept :a))
;;       
;; This will expand to
;;
;;     (-> 
;;       (nfa*
;;         [:init \a :a \b :b]
;;         [:b \a :b])
;;       (accept-in :a))
;;
;; Similarly for DFAs.

(defmacro new-fsm
  "Create new FSM based on the given type and a series of states."
  [type & states]
  (let [create-fsm (case type
                     :nfa `nfa*
                     :dfa `dfa*
                     `dfa*)
        accept-state? (fn [[k & _]] 
                        (= k :accept))
        state-lists (map (fn [[_ s & t]]
                           `(list* ~s (state* ~type ~s ~@t))) states)
        accept-states `(accept-in ~@(->> 
                                      (filter accept-state? states)
                                      (map second)))]
  `(->
     (~create-fsm
        ~@state-lists)
      ~accept-states)))

(defmacro nfa
  "Create NFA from series of states."
  [& states]
  `(new-fsm :nfa ~@states))

(defmacro dfa
  "Create DFA from series of states."
  [& states]
  `(new-fsm :dfa ~@states))
