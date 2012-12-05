(ns ^{ :doc "DFA Combination/Transformation"
       :author "Yannick Scherer" }
  lexington.fsm.dfa
  (:use [lexington.fsm.nfa :as nfa]
        [lexington.fsm.states :as s :only [accept! reject!]]
        [lexington.fsm.transitions :as t :only [any]]
        [lexington.fsm.errors :as e]
        [lexington.fsm.utils :only [fsm-reindex fsm-remove-unreachable-states]]
        lexington.fsm.core))

;; ## DFA Structure
;;
;; DFAs have a single destination state for each state and input.

(defn dfa-reject-state?
  [s]
  (= s s/reject!))

(defn dfa-accept-state?
  [s]
  (= s s/accept!))

;; ## Simple Transformations

(defn invert-dfa
  "Create a DFA that does accept everything not accepted by the given one. This
   is done by switching the accepting states with all non-accepting states."
  [{:keys[accept states] :as dfa}]
  (-> dfa
    (assoc :accept (set (filter (comp not (set accept)) states)))))

;; ## Cartesian Product

(defn- cartesian-product-transitions
  "Create transition map for cartesian product."
  [t1 t2]
  (into {}
    (for [x (keys t1) y (keys t2)]
      (let [tx (t1 x)
            ty (t2 y)
            in (set (concat (keys tx) (keys ty)))]
        (vector [x y]
                (into {}
                      (for [e in] 
                        (let [txe (or (tx e) (tx t/any))
                              tye (or (ty e) (ty t/any))]
                          (when (or (set? txe) (set? tye))
                            (e/error "This operation only works with DFAs."))
                          (when (and txe tye)
                            (vector e [txe tye]))))))))))

(defn cartesian-product-dfa
  "Build the cartesian product of two DFAs. This is done by using the following data:
- States: `(:states fsm1)` X `(:states fsm2)`
- Initial State: (`(:initial fsm1)`, `(:initial fsm2)`)
- Transitions: { [x, y] { <input> [(get-in fsm1 [:transitions x <input>]), ...] ... } ... }`
  "
  [fsm1 fsm2 accept?]

  ;; New states and unique names for them
  (let [{ s1 :states i1 :initial t1 :transitions a1 :accept r1 :reject } fsm1
        { s2 :states i2 :initial t2 :transitions a2 :accept r2 :reject } fsm2]
    (let [states (for [x s1 y s2] [x y])
          initial [i1 i2]
          accept (filter (fn [[x y]]
                           (accept? x y a1 a2))
                         states)
          transitions (cartesian-product-transitions t1 t2)]
      (-> {}
        (assoc :states (set states))
        (assoc :initial initial)
        (assoc :transitions transitions)
        (assoc :accept (set accept))
        (assoc :reject #{})
        fsm-remove-unreachable-states
        (fsm-reindex #(= % [s/reject! s/reject!])
                     #(= % [s/accept! s/accept!]))))))

(def intersect-dfa
  "Create (cartesian product) intersection of DFAs."
  (letfn [(intersect-acceptor? [x y a1 a2]
            (and (a1 x) (a2 y)))]
    (fn [fsm1 & fsms]
      (reduce
        #(cartesian-product-dfa %1 %2 intersect-acceptor?)
        fsm1
        fsms))))

(def union-dfa
  "Create (cartesian product) union of DFAs."
  (letfn [(union-acceptor? [x y a1 a2]
            (or (a1 x) (a2 y)))]
    (fn [fsm1 & fsms]
      (reduce
        #(cartesian-product-dfa %1 %2 union-acceptor?)
        fsm1
        fsms))))

(defn diff-fsm
  "Create (cartesian product) difference of two DFAs."
  [fsm1 fsm2]
  (intersect-dfa fsm1 (invert-dfa fsm2)))

;; ## Combinations (using NFA)

(defn concat-dfa
  "Concatenate a series of NFAs/DFAs, creating a DFA."
  [& dfas]
  (let [nfas (map nfa/dfa->nfa dfas)]
    (nfa/nfa->dfa (apply nfa/concat-nfa nfas))))

(defn union-dfa
  "Create union of a series of NFAs/DFAs, creating a DFA."
  [& dfas]
  (let [nfas (map nfa/dfa->nfa dfas)]
    (nfa/nfa->dfa (apply nfa/union-nfa nfas))))

(defn loop-dfa
  "Create looping DFA from given DFA."
  [dfa]
  (-> dfa
    nfa/dfa->nfa
    nfa/loop-nfa
    nfa/nfa->dfa))

(defn loop0-dfa
  "Create looping DFA from given DFA that also accepts empty inputs."
  [dfa]
  (-> dfa
    nfa/dfa->nfa
    nfa/loop0-nfa
    nfa/nfa->dfa))
