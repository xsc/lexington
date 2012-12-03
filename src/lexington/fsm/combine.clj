(ns 
  lexington.fsm.combine
  (:use [lexington.fsm.states :as s]
        [lexington.fsm.transitions :as t :only [any]]
        [lexington.fsm.errors :as e :only [error]]
        lexington.fsm.helpers
        lexington.fsm.transform
        [lexington.fsm.minimize :only [minimize-dfa]]))

;; ## Cartesian Product (only DFA)

(defn- dfa-cartesian-product-transitions
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

(defn- dfa-cartesian-product
  "Build the cartesian product of two DFAs. This is done by using the following data:
- States: `(:states fsm1)` X `(:states fsm2)`
- Initial State: (`(:initial fsm1)`, `(:initial fsm2)`)
- Transitions: { [x, y] { <input> [(get-in fsm1 [:transitions x <input>]), ...] ... } ... }`
  "
  ([fsm1 fsm2 accept?] (dfa-cartesian-product fsm1 fsm2 accept? { :minimize true }))
  ([fsm1 fsm2 accept? option-map]

  ;; New states and unique names for them
  (let [{ s1 :states i1 :initial t1 :transitions a1 :accept r1 :reject } fsm1
        { s2 :states i2 :initial t2 :transitions a2 :accept r2 :reject } fsm2]
    (let [states (for [x s1 y s2] [x y])
          initial [i1 i2]
          accept (filter (fn [[x y]]
                           (accept? x y a1 a2))
                         states)
          transitions (dfa-cartesian-product-transitions t1 t2)
          finalizer (if (option-map :minimize) minimize-dfa identity)]
      (-> {}
        (assoc :states (set states))
        (assoc :initial initial)
        (assoc :transitions transitions)
        (assoc :accept (set accept))
        (assoc :reject #{})
        remove-unreachable-states
        (reindex-fsm #(= % [s/reject! s/reject!])
                     #(= % [s/accept! s/accept!]))
        (finalizer))))))

;; ## Logical Combinations (DFA only)

(def intersect-fsm
  "Create (cartesian product) intersection of FSMs."
  (letfn [(intersect-acceptor? [x y a1 a2]
            (and (a1 x) (a2 y)))]
    (fn [fsm1 & fsms]
      (reduce
        #(dfa-cartesian-product %1 %2 intersect-acceptor?)
        fsm1
        fsms))))

(def union-fsm
  "Create (cartesian product) union of FSMs."
  (letfn [(union-acceptor? [x y a1 a2]
            (or (a1 x) (a2 y)))]
    (fn [fsm1 & fsms]
      (reduce
        #(dfa-cartesian-product %1 %2 union-acceptor?)
        fsm1
        fsms))))

(defn diff-fsm
  "Create (cartesian product) difference of two FSMs."
  [fsm1 fsm2]
  (intersect-fsm fsm1 (invert-fsm fsm2)))

;; ## Concatenation

(defn- merge-transitions
  "Merge two NFA transition tables, creating an NFA transition table. 
   States with the same name will be merged."
  [t1 t2]
  (merge-with
    (fn [tx ty]
      (merge-with (comp set concat) tx ty))
    t1 t2))

(defn- build-concat-nfa
  [fsm1 fsm2]
  (let [{ a1 :accept r1 :reject s1 :states i1 :initial t1 :transitions } (dfa->nfa fsm1)
        { a2 :accept r2 :reject s2 :states i2 :initial t2 :transitions } (dfa->nfa fsm2)
        ext-t2 (reduce (fn [t s]
                         (assoc t s (t2 i2))) t2 a1)
        transitions (merge-transitions t1 ext-t2)
        accept (set (concat (disj a2 i2) (when (a2 i2) a1)))
        reject (set (concat r1 (disj r2 i2) (when (r2 i2) a1)))
        states (set (concat s1 (disj s2 i2)))
        initial i1]
    (assoc {}
           :accept accept
           :reject reject
           :states states
           :initial initial
           :transitions transitions)))


(def concat-fsm
  "Create concatenation of FSMs. This is done by merging the accept states
   of an FSM with the initial state of the next FSM. Non-determinism has to
   be resolved."
  (letfn [(conc-fsm [fsm1 fsm2]
            (-> (build-concat-nfa fsm1 fsm2)
              (nfa->dfa)
              (minimize-dfa)))]
    (fn [& fsms]
      (let [fsms (map-indexed (fn [i f] 
                                (prefix-fsm-states 
                                  (remove-unreachable-states f) 
                                  (str "x" i "-"))) fsms)]
        (reduce 
          #(conc-fsm %1 %2)
          fsms)))))

;; ## Loop (Kleene Star)

(defn loop-fsm
  "Create looping FSM by merging the initial state with all accepting states.
   Non-determinism is resolved using nfa->dfa."
  [fsm]
  (let [{:keys[transitions accept initial] :as nfa} (dfa->nfa fsm)
        initial-transitions (transitions initial)]
    (-> nfa
      (assoc :transitions
             (reduce
               (fn [t [from tt]]
                 (assoc t from
                   (if (accept from)
                     (merge-with (comp set concat)
                                 tt
                                 initial-transitions)
                     tt)))
               {}
               transitions))
      (nfa->dfa)
      (minimize-dfa))))
