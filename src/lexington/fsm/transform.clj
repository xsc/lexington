(ns ^{ :doc "FSM Transformation and Combination"
       :author "Yannick Scherer" }
  lexington.fsm.transform
  (:use [lexington.fsm.transitions :as t :only [any]]
        [lexington.fsm.states :as s :only [reject! accept!]]
        [lexington.fsm.errors :as e :only [error]]
        lexington.fsm.helpers))

;; ## Analysis (NFA/DFA)

(defn state-seq-bfs
  "Create a breadth-first seq of all the states reachable from the given node."
  ([fsm] (state-seq-bfs fsm (:initial fsm)))
  ([{:keys[transitions]} root]
   (loop [next-states [root]
          visited-states []]
     (let [visited-states (concat visited-states next-states)]
       (if-not (seq next-states)
         visited-states
         (let [child-states (filter (comp not (set visited-states))
                                    (mapcat #(mapcat (fn [x]
                                                       (if (set? x)
                                                         x
                                                         (vector x)))
                                                      (vals (transitions %))) next-states))]
           (recur child-states visited-states)))))))

(defn state-seq-dfs
  "Create a depth-first seq of all the states reachable from the given node."
  ([fsm] (state-seq-dfs fsm (:initial fsm)))
  ([{:keys[transitions]} root]
   (loop [next-states [root]
          visited-states []]
     (if-not (seq next-states)
       visited-states
       (let [[s & rst] next-states
             child-states (filter (comp not (set (cons s visited-states)))
                                  (mapcat (fn [x]
                                            (if (set? x) x (vector x)))
                                          (vals (transitions s))))
             rest-states (filter (comp not (set child-states)) rst)]
         (recur (concat child-states rest-states)
                (concat visited-states [s])))))))

(defn reachable-states
  "Get a set of all reachable states based on an initial state and a transition map."
  [fsm root]
  (set (state-seq-bfs fsm root)))

(defn unreachable-states
  "Get a set of all unreachable states based on a set of all states, an initial one and
   a transition map."
  [{:keys[states] :as fsm} root]
  (let [reachable (reachable-states fsm root)]
    (set (filter (comp not reachable) states))))

;; ## Transformations (NFA/DFA)

(defn invert-fsm
  "Create an FSM that does accept everything not accepted by the given FSM. This
   is done by switching the accepting states with all non-accepting states. Attention:
   rejecting states will be cleared."
  [{:keys[accept states] :as fsm}]
  (-> fsm
    (assoc :accept (set (filter (comp not accept) states)))
    (assoc :reject #{})))

(defn rename-fsm-states
  "Rename FSM states using the given function. If the function returns nil,
   the current name will be kept."
  [{:keys[accept reject states transitions initial] :as fsm} rename-fn]
  (letfn [(rename [x] (or (rename-fn x) x))]
    (assoc fsm
      :accept (set (map rename accept))
      :reject (set (map rename reject))
      :states (set (map rename states))
      :initial (rename initial)
      :transitions (reduce (fn [m [s t]]
                             (assoc m 
                                    (rename s)
                                    (reduce 
                                      (fn [tr [e to]]
                                        (if (set? to)
                                          (assoc tr e (set (map rename to)))
                                          (assoc tr e (rename to))))
                                      {}
                                      t)))
                           {}
                           transitions))))

(defn prefix-fsm-states
  "Prefix all states of an FSM with a given string."
  [fsm prefix]
  (rename-fsm-states fsm
    (fn [x]
      (when-not (or (= x s/accept!) (= x s/reject!))
        (keyword (str prefix (name x)))))))

(defn remove-unreachable-states
  "Remove all unreachable states from an FSM."
  [{:keys[initial transitions accept reject states] :as fsm}]
  (let [reachable? (reachable-states fsm initial)]
    (assoc fsm
      :accept (set (filter reachable? accept))
      :reject (set (filter reachable? reject))
      :states (set (filter reachable? states))
      :transitions (reduce (fn [m [s t]]
                             (if (reachable? s)
                               (assoc m s t)
                                m))
                           {}
                           transitions))))

(defn reindex-fsm
  "Rename states to `state-x` where `state-0` is the initial state. The higher the
   given number the further away from the root node is a state."
  ([fsm] (reindex-fsm fsm #(= % s/reject!) #(= % s/accept!)))
  ([{:keys[accept reject states transitions initial] :as fsm} reject-state? accept-state?]
   (let [state-map (zipmap 
                     (filter (comp not reject-state?) (state-seq-bfs fsm initial))
                     (for [i (range)] (keyword (str "state-" i))))]
     (rename-fsm-states fsm (fn [s] 
                              (cond (reject-state? s) s/reject!
                                    (accept-state? s) s/accept!
                                    :else (state-map s)))))))

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
  [fsm1 fsm2 accept?]

  ;; New states and unique names for them
  (let [{ s1 :states i1 :initial t1 :transitions a1 :accept r1 :reject } fsm1
        { s2 :states i2 :initial t2 :transitions a2 :accept r2 :reject } fsm2]
    (let [states (for [x s1 y s2] [x y])
          initial [i1 i2]
          accept (filter (fn [[x y]]
                           (accept? x y a1 a2))
                         states)
          transitions (dfa-cartesian-product-transitions t1 t2)]
      (-> {}
        (assoc :states (set states))
        (assoc :initial initial)
        (assoc :transitions transitions)
        (assoc :accept (set accept))
        (assoc :reject #{})
        remove-unreachable-states
        (reindex-fsm #(= % [s/reject! s/reject!])
                     #(= % [s/accept! s/accept!]))))))

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

;; ## NFA to DFA
;;
;; An NFA is an FSM not complying to Lexington's FSM specification since its
;; transition table contains sets of target states instead of single states.
;;
;; NFAs cannot be used with cartesian product functions unless they are converted
;; to DFAs first. Also, there is no epsilon here.

(defn dfa->nfa
  "Convert DFA to NFA by replacing target states with sets containing the single
   target state."
  [{:keys[transitions] :as dfa}]
  (let [transitions* (-> transitions
                       (remap
                         (fn [to-table]
                           (-> to-table
                             (remap 
                               (fn [to]
                                 (if (set? to)
                                   to
                                   (hash-set to))))
                             (into-map))))
                       (into-map))]
    (assoc dfa :transitions transitions*)))

(defn nfa->dfa
  "Convert NFA to DFA by creating 'set-states' and transitions between them."
  [{:keys[accept reject initial transitions] :as nfa}]
  (letfn [(next-state [current input]
            (let [current (if (vector? current) current (vector current))]
              (vec (apply sorted-set
                     (mapcat 
                       (fn [s]
                         (let [tt (transitions s)]
                           (or (tt input) (tt t/any))))
                       current)))))
          (get-transitions [current]
            (let [inputs (mapcat #(keys (transitions %)) current)]
              (reduce
                (fn [t input]
                  (assoc t input (next-state current input)))
                {}
                inputs)))]
    (loop [states      #{[initial]}
           transitions (assoc {} [initial] (transitions initial))
           remaining-states [[initial]]]
      (if-not (seq remaining-states)
        (-> {}
          (assoc :initial [initial])
          (assoc :transitions transitions)
          (assoc :states states)
          (assoc :accept (set (filter (fn [x] (some accept x)) states)))
          (reindex-fsm 
            (fn [x]
              (not (some (comp not #(= % s/reject!)) x)))
            (fn [x]
              (not (some (comp not #(= % s/accept!)) x)))))
        (let [child-transitions (map get-transitions remaining-states)
              child-states (filter (comp not states) (mapcat vals child-transitions))]
          (recur (set (concat states child-states))
                 (merge transitions (zipmap remaining-states child-transitions))
                 child-states))))))

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
              (nfa->dfa)))]
    (fn [& fsms]
      (let [fsms (map-indexed (fn [i f] 
                                (prefix-fsm-states 
                                  (remove-unreachable-states f) 
                                  (str "x" i "-"))) fsms)]
        (reduce 
          #(conc-fsm %1 %2)
          fsms)))))
