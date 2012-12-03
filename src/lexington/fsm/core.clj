(ns ^{ :doc "Lexington FSM Representation"
       :author "Yannick Scherer" }
  lexington.fsm.core
  (:require [lexington.fsm.states :as s]
            [lexington.fsm.transitions :as t]
            [lexington.fsm.errors :as e]
            [lexington.fsm.fsm :as fsm]
            [lexington.fsm.nfa :as n :only [nfa*]]
            [lexington.fsm.dfa :as d :only [dfa*]]))

;; ## Combining of Transitions 

(defn dfa-combine
  [a b]
  a)

(defn nfa-combine
  [a b]
  (let [a-set (if (set? a) a (hash-set a))
        b-set (if (set? b) b (hash-set b))]
    (set (concat a-set b-set))))

(def ^:dynamic *transition-combine* dfa-combine)

;; ## Transition DSL

(defmacro transitions*
  "The `transitions*` macro can be used for state generation. It introduces shorthands for the 
different special transitions and target states:

- `(:not ...)` represents the `(except ...)` transition
- `(:or ...)` represents the `(one-of ...)` transition
- `:accept!` represents the `accept!` destination state
- `:reject!` represents the `reject!` destination state
- `_` represents the `any` transition or the `continue!` destination state
Additionally, the input entity and the next state are now separated by an arrow `->`. Example:
    
    (transitions*
      (:not 'a 'b 'c) -> :a
      (:or 'b 'c)     -> :accept!
      _               -> :f)

  "
  [k & transitions]
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
          (resolve-destination [d x]
            (cond (is-underscore? d) x
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
                               (resolve-destination next-state k)))
                     (partition 3 transitions)))
         *transition-combine*))))

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
  `(->
     (~(if (= type :nfa) `n/nfa* `d/dfa*)
       ~@(map (fn [[_ s & t]]
                `(binding [*transition-combine* ~(if (= type :nfa) nfa-combine dfa-combine)]
                   (list* ~s (transitions* ~s ~@t))))
              states))
     ~@(map (fn [[_ s & _]]
              `(fsm/accept-in ~s))
            (filter (fn [[k & _]] 
                      (= k :accept-state)) states))))

(defmacro nfa
  "Create NFA from series of states."
  [& states]
  `(new-fsm :nfa ~@states))

(defmacro dfa
  "Create DFA from series of states."
  [& states]
  `(new-fsm :dfa ~@states))
