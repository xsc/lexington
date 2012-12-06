(ns ^{ :doc "DFA Combination/Transformation"
       :author "Yannick Scherer" }
  lexington.fsm.dfa
  (:use [lexington.fsm.nfa :as n]
        [lexington.fsm.states :as s :only [accept! reject!]]
        [lexington.fsm.transitions :as t :only [any]]
        [lexington.fsm.errors :as e]
        lexington.fsm.core
        lexington.fsm.utils))

;; ## DFA Structure
;;
;; DFAs have a single destination state for each state and input.

(defn dfa-reject-state?
  [s]
  (= s #{s/reject!}))

(defn dfa-accept-state?
  [s]
  (= s #{s/accept!}))

;; ## Simple Transformations

(defn- prepare-fsm
  "Prepare FSM to enable unified state handling in cartesian product, inversion, etc...
   Makes e.g. checks if the `reject!` state is available unnecessary."
  [fsm]
  (-> fsm
    n/nfa->dfa
    fsm-normalize))

(defn invert-dfa
  "Create a DFA that does accept everything not accepted by the given one. This
   is done by switching the accepting states with all non-accepting states and replacing
   the `reject!` state with the `accept!` one."
  [{:keys[accept states] :as dfa}]
  (-> dfa
    prepare-fsm
    (fsm-rename-single-state s/reject! s/accept!)
    (assoc :reject #{})
    (assoc :accept (->> states
                     (filter (comp not (set accept)))
                     (cons s/accept!)
                     set))))

;; ## Cartesian Product

(defn- cartesian-product-transitions
  "Create transition map for cartesian product."
  [t1 t2]
  (into {}
    (for [x (keys t1) y (keys t2)]
      (let [tx (t1 x)
            ty (t2 y)
            in (set (concat (keys tx) (keys ty)))]
        (->>
          (for [e in] 
            (let [txe (or (tx e) (tx t/any) #{s/reject!})
                  tye (or (ty e) (ty t/any) #{s/reject!})]
              (vector e (hash-set [(first txe) (first tye)]))))
          (into {})
          (vector [x y]))))))

(defn cartesian-product-dfa
  "Build the cartesian product of two DFAs. This is done by using the following data:
- States: `(:states fsm1)` X `(:states fsm2)`
- Initial State: (`(:initial fsm1)`, `(:initial fsm2)`)
- Transitions: { [x, y] { <input> [(get-in fsm1 [:transitions x <input>]), ...] ... } ... }`
  "
  ([fsm1 fsm2] (cartesian-product-dfa fsm1 fsm2 (constantly nil) (constantly nil)))
  ([fsm1 fsm2 accept? reject?]
   (let [{ s1 :states i1 :initial t1 :transitions a1 :accept r1 :reject } (prepare-fsm fsm1)
         { s2 :states i2 :initial t2 :transitions a2 :accept r2 :reject } (prepare-fsm fsm2)]

     ;; Create set-states
     (let [states (for [x s1 y s2] [x y])
           initial [i1 i2]
           accept (filter (fn [[x y]] (accept? x y (set a1) (set a2))) states)
           reject (filter (fn [[x y]] (accept? x y (set r1) (set r2))) states)
           transitions (cartesian-product-transitions t1 t2)]
       (-> {}
         (assoc :states (set states))
         (assoc :accept (set accept))
         (assoc :reject (set reject))
         (assoc :initial initial)
         (assoc :transitions transitions)
         fsm-remove-unreachable-states
         (fsm-reindex #(= % [s/reject! s/reject!])
                      #(= % [s/accept! s/accept!])))))))

(def cartesian-intersection-dfa
  "Create (cartesian product) intersection of DFAs."
  (letfn [(intersect-acceptor? [x y a1 a2]
            (and (a1 x) (a2 y)))
          (intersect-rejector? [x y r1 r2]
            (or (r1 x) r2 y))]
    (fn [fsm1 & fsms]
      (reduce
        #(cartesian-product-dfa 
           %1 %2 
           intersect-acceptor?
           intersect-rejector?)
        fsm1
        fsms))))

(def cartesian-union-dfa
  "Create (cartesian product) union of DFAs."
  (letfn [(union-acceptor? [x y a1 a2]
            (or (a1 x) (a2 y)))
          (union-rejector? [x y r1 r2]
            (and (r1 x) (r2 y)))]
    (fn [fsm1 & fsms]
      (reduce
        #(cartesian-product-dfa 
           %1 %2 
           union-acceptor?
           union-rejector?)
        fsm1
        fsms))))

(defn cartesian-difference-dfa
  "Create (cartesian product) difference of two DFAs."
  [fsm1 fsm2]
  (cartesian-intersection-dfa fsm1 (invert-dfa fsm2)))

;; ## Combinations (using NFA)

(defn- dfa-fn
  "Wrapper around a NFA function to let it produce a DFA."
  [f]
  (fn [& args]
    (->
      (apply f args)
      nfa->dfa)))

(def concat-dfa
  "Concatenate a series of NFAs/DFAs, creating a DFA."
  (dfa-fn n/concat-nfa))

(def union-dfa
  "Create union of a series of NFAs/DFAs, creating a DFA."
  (dfa-fn n/union-nfa))

(def loop-dfa
  "Create looping DFA from given DFA."
  (dfa-fn n/loop-nfa))

(def loop0-dfa
  "Create looping DFA from given DFA that also accepts empty inputs."
  (dfa-fn n/loop0-nfa))
